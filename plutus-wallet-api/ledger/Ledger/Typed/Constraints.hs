{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
module Ledger.Typed.Constraints(
  TypedTxConstraints
  , emptyTypedTxConstraints
  , TypedOrPubKeyTxIns(..)
  , TypedOrUntypedTxOuts(..)
  , toTypedTxConstraints
  , Ledger.Typed.Constraints.addTypedTxIn
  , Ledger.Typed.Constraints.addTypedTxOut
  , toUntypedLedgerConstraints
  -- * 'TypedTxConstraints' with either side existentially quantified
  , TypedTxSomeIns(..)
  , TypedTxSomeOuts(..)
  , addSomeTypedTxIn
  , addSomeTypedTxOut
  , addManyTypedTxIns
  ) where

import           Data.Either                 (partitionEithers)
import           Data.Kind
import           Data.List                   (foldl')
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Ledger.Constraints
import           Ledger.Constraints.OnChain  (OnChainUtxo (..))
import           Ledger.Constraints.UtxoLike (UtxoLike (..))
import           Ledger.Scripts              (DataValue (..))
import           Ledger.Tx                   (TxIn, TxOut)
import           Ledger.Typed.Scripts        (ScriptInstance, ScriptType (..))
import           Ledger.Typed.Tx
import           Ledger.Typed.TypeUtils

import           Language.PlutusTx

data TypedOrPubKeyTxIns (ins :: [Type]) =
  TypedOrPubKeyTxIns
    { topTypedTxIns  :: HListF TypedScriptTxIn ins
    , topPubKeyTxIns :: [PubKeyTxIn]
    }

untypedInputs :: forall ins. TypedOrPubKeyTxIns ins -> Set TxIn
untypedInputs TypedOrPubKeyTxIns{topTypedTxIns, topPubKeyTxIns} =
  Set.fromList (hfOut tyTxInTxIn topTypedTxIns)
  <> Set.fromList (unPubKeyTxIn <$> topPubKeyTxIns)

addTypedTxIn'
  :: forall (ins :: [Type]) inn
  .  TypedScriptTxIn inn
  -> TypedOrPubKeyTxIns ins
  -> TypedOrPubKeyTxIns (inn ': ins)
addTypedTxIn' txi t =
  t { topTypedTxIns = HConsF txi (topTypedTxIns t) }

data TypedOrUntypedTxOuts (outs :: [Type]) =
  TypedOrUntypedTxOuts
    { totTypedTxOuts :: HListF TypedScriptTxOut outs
    , totOtherOuts   :: [TxOut]
    }

addTypedTxOut'
  :: forall (outs :: [Type]) out
  .  TypedScriptTxOut out
  -> TypedOrUntypedTxOuts outs
  -> TypedOrUntypedTxOuts (out ': outs)
addTypedTxOut' txo t =
  t { totTypedTxOuts = HConsF txo (totTypedTxOuts t) }

data TypedTxSomeIns (outs :: [Type]) =
  forall ins. TypedTxSomeIns (TypedTxConstraints ins outs)

addSomeTypedTxIn
  :: forall (outs :: [Type]) (newIn :: *)
  . TypedScriptTxIn newIn
  -> TypedTxSomeIns outs
  -> TypedTxSomeIns outs
addSomeTypedTxIn inn (TypedTxSomeIns tx) =
  TypedTxSomeIns $ addTypedTxIn inn tx

-- | Adds many homogeneous 'TypedScriptTxIn' to a 'TypedTx'.
addManyTypedTxIns
    :: forall (ins :: [Type]) (outs :: [Type]) (newIn :: Type)
    . [TypedScriptTxIn newIn]
    -> TypedTxConstraints ins outs
    -> TypedTxSomeIns outs
addManyTypedTxIns ins tx = foldl' (\someTx inn -> addSomeTypedTxIn inn someTx) (TypedTxSomeIns tx) ins

-- | A wrapper around a 'TypedTx' that hides the output list type as an existential parameter.
data TypedTxSomeOuts (ins :: [Type]) = forall outs . TypedTxSomeOuts (TypedTxConstraints ins outs)

-- | Add a 'TypedScriptTxOut' to a 'TypedTxSomeOuts'. Note that we do not have to track
-- the output connection types explicitly.
addSomeTypedTxOut
    :: forall (ins :: [Type]) (newOut :: *)
    . TypedScriptTxOut newOut
    -> TypedTxSomeOuts ins
    -> TypedTxSomeOuts ins
addSomeTypedTxOut out (TypedTxSomeOuts tx) = TypedTxSomeOuts $ addTypedTxOut out tx

untypedOutputs :: forall outs. TypedOrUntypedTxOuts outs -> [TxOut]
untypedOutputs TypedOrUntypedTxOuts{totTypedTxOuts, totOtherOuts} =
  hfOut tyTxOutTxOut totTypedTxOuts ++ totOtherOuts

untypedDataValues :: forall outs. TypedOrUntypedTxOuts outs -> [DataValue]
untypedDataValues TypedOrUntypedTxOuts{totTypedTxOuts} = hfOut dv totTypedTxOuts where
  dv TypedScriptTxOut{tyTxOutData} = DataValue (toData tyTxOutData)

type TypedTxConstraints (ins :: [Type]) (outs :: [Type]) =
  TxConstraints (TypedOrPubKeyTxIns ins) (TypedOrUntypedTxOuts outs)

emptyTypedTxConstraints :: TypedTxConstraints '[] '[]
emptyTypedTxConstraints =
  (mempty @(TxConstraints () ())) { tcInputs = TypedOrPubKeyTxIns HNilF [], tcOutputs = TypedOrUntypedTxOuts HNilF [] }

-- | Turn an on-chain 'PendingTxConstraints' value into an (off-chain)
--   'TypedTxConstraints '[]' value with either zero or one
--   continuing outputs. Returns 'Nothing' if the self-payment is overspecified
--   (more than one output to the contract's own address). All outputs other
--   than the self-payment remain untyped.
toTypedTxConstraints
  :: forall inn
  .  (IsData (DataType inn))
  => ScriptInstance inn
  -> PendingTxConstraints (DataType inn)
  -> Maybe (Either (TypedTxConstraints '[] '[]) (TypedTxConstraints '[] '[inn]))
toTypedTxConstraints inst txc  =
  let txIns = TypedOrPubKeyTxIns { topTypedTxIns = HNilF, topPubKeyTxIns = [] }
      mpUtxo = \case
        OnChainPayToSelf a vl      -> Left (a, vl)
        OnChainPayToScript vh dv v -> Right $ scriptUtxo vh dv v
        OnChainPayToPubKey pk vl   -> Right $ pubKeyUtxo pk vl
  in case partitionEithers (mpUtxo <$> tcOutputs txc) of
    ([], xs) -> Just $ Left $ txc { tcInputs = txIns, tcOutputs = TypedOrUntypedTxOuts HNilF xs }
    ([(a, vl)], xs) ->
      let out = makeTypedScriptTxOut inst a vl
      in Just $ Right $ txc { tcInputs = txIns, tcOutputs = TypedOrUntypedTxOuts (HConsF out HNilF) xs }
    -- We don't support more than one 'OnChainPayToSelf' output
    _ -> Nothing

-- | Add a typed transaction input to the typed constraints
addTypedTxIn
  :: forall (ins :: [Type]) (outs :: [Type]) inn
  .  TypedScriptTxIn inn
  -> TypedTxConstraints ins outs
  -> TypedTxConstraints (inn ': ins) outs
addTypedTxIn inn tc =
  let oldIns = tcInputs tc
      newIns :: TypedOrPubKeyTxIns (inn ': ins)
      newIns = addTypedTxIn' inn oldIns
  in tc { tcInputs = newIns }

-- | Add a typed transaction output to the typed constraints
addTypedTxOut
  :: forall (ins :: [Type]) (outs :: [Type]) out
  .  TypedScriptTxOut out
  -> TypedTxConstraints ins outs
  -> TypedTxConstraints ins (out ': outs)
addTypedTxOut out tc =
  let oldOuts = tcOutputs tc
      newOuts = addTypedTxOut' out oldOuts
  in tc { tcOutputs = newOuts }

-- | Turn the typed tx constraints into ledger tx constraints
toUntypedLedgerConstraints
  :: forall inn out
  .  TypedTxConstraints inn out
  -> LedgerTxConstraints
toUntypedLedgerConstraints txc =
  txc
    { tcInputs  = Set.toList (untypedInputs (tcInputs txc))
    , tcOutputs = untypedOutputs (tcOutputs txc)
    , tcDataValues = tcDataValues txc ++ untypedDataValues (tcOutputs txc)
    }

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Ledger.Constraints.OnChain where

import qualified Data.Aeson                       as Aeson
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                     (Generic)

import           Language.PlutusTx                (IsData (..))
import qualified Language.PlutusTx                as PlutusTx
import           Language.PlutusTx.Prelude

import           Ledger.Constraints.TxConstraints
import           Ledger.Constraints.UtxoLike
import           Ledger.Crypto                    (PubKey)
import           Ledger.Interval                  (contains)
import           Ledger.Scripts                   (DataValue (..), ValidatorHash)
import           Ledger.Tx                        (TxOutType (..))
import           Ledger.Validation                (PendingTx, PendingTx' (..), TxOut (..))
import qualified Ledger.Validation                as V
import           Ledger.Value                     (Value, leq)

import qualified Prelude                          as Haskell

-- TODO: Use something other than unit for the input constraints (need
-- to think about representation)
-- | Constraints on a transaction in on-chain code.
type PendingTxConstraints a = TxConstraints () [OnChainUtxo a]

data OnChainUtxo a =
    OnChainPayToPubKey PubKey Value
    | OnChainPayToScript ValidatorHash DataValue Value
    | OnChainPayToSelf a Value
    deriving stock (Haskell.Eq, Generic, Haskell.Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance UtxoLike (OnChainUtxo a) where
    pubKeyUtxo = OnChainPayToPubKey
    scriptUtxo = OnChainPayToScript

instance Pretty a => Pretty (OnChainUtxo a) where
  pretty = \case
    OnChainPayToPubKey pk vl -> hang 2 $ vsep ["Pay to pubkey:", pretty pk, pretty vl]
    OnChainPayToScript vh dh vl -> hang 2 $ vsep ["Pay to script:", pretty vh, pretty dh, pretty vl]
    OnChainPayToSelf a vl -> hang 2 $ vsep ["Pay to self:", pretty a, pretty vl]

{-# INLINABLE payToOwnAddress #-}
-- | Require the transaction to pay the value to the contract's own
--   address
payToOwnAddress :: forall i a. (Monoid i, IsData a) => Value -> a -> TxConstraints i [OnChainUtxo a]
payToOwnAddress vl a =
    (mempty @(TxConstraints i [OnChainUtxo a]))
        { tcOutputs = [OnChainPayToSelf a vl]
        , tcDataValues = [DataValue (toData a)]
        }

-- | Check whether an 'OnChainUtxo' value is present in the outputs of
--   the 'PendingTx'
checkOnChainTxOut :: (IsData a) => PendingTx -> OnChainUtxo a -> Bool
checkOnChainTxOut ptx =
    let valueLockedWith dataValue outs =
            let aggregate dh = foldMap snd (filter ((==) dh . fst) outs)
            in fmap aggregate (V.findDataHash dataValue ptx)
    in \case
        OnChainPayToPubKey pk vl ->
            traceIfFalseH "OnChainPayToPubKey" (vl `leq` V.valuePaidTo ptx pk)
        OnChainPayToScript addr dv vl ->
            case valueLockedWith dv (V.scriptOutputsAt addr ptx) of
                Nothing    -> traceH "Missing data hash" False
                Just total -> traceIfFalseH "PayToScript" (vl `leq` total)
        OnChainPayToSelf a vl ->
            let outs = mapMaybe (\case { TxOut{txOutType=PayToScript ds, txOutValue} -> Just (ds, txOutValue); _ -> Nothing }) (V.getContinuingOutputs ptx) in
            case valueLockedWith (DataValue (toData a)) outs of
                Nothing    -> traceH "Missing data hash (pay to self)" False
                Just total -> traceIfFalseH "Pay to self" (vl `leq` total)

{-# INLINABLE checkPendingTx #-}
-- | Does the 'PendingTx' satisfy the constraints?
checkPendingTx :: (IsData a) => PendingTxConstraints a -> PendingTx -> Bool
checkPendingTx TxConstraints{tcOutputs, tcDataValues, tcForge, tcInterval, tcRequiredSignatures, tcValueMoved} ptx =
    let outputsOK = all (checkOnChainTxOut ptx) tcOutputs
        dataValuesOK = all (`elem` fmap snd (pendingTxData ptx)) tcDataValues
        forgeOK = tcForge == pendingTxForge ptx
        valueMovedOK = tcValueMoved `leq` V.valueSpent ptx
        intervalOK = tcInterval `contains` pendingTxValidRange ptx
        signaturesOK = all (V.txSignedBy ptx) tcRequiredSignatures
    in traceIfFalseH "checkPendingTx failed - outputs not OK" outputsOK
        && traceIfFalseH "checkPendingTx failed - data values not OK" dataValuesOK
        && traceIfFalseH "checkPendingTx failed - forge not OK" forgeOK
        && traceIfFalseH "checkPendingTx failed - value moved not OK" valueMovedOK
        && traceIfFalseH "checkPendingTx failed - interval not OK" intervalOK
        && traceIfFalseH "checkPendingTx failed - signatures missing" signaturesOK

PlutusTx.makeIsData ''OnChainUtxo
PlutusTx.makeLift ''OnChainUtxo

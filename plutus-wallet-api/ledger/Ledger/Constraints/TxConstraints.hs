{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | Constraints for transactions
module Ledger.Constraints.TxConstraints where

import qualified Data.Aeson                  as Aeson
import           Data.Bifunctor              (Bifunctor (..))
import           Data.Text.Prettyprint.Doc   hiding ((<>))
import           GHC.Generics                (Generic)

import           IOTS                        (IotsType)

import qualified Language.PlutusTx           as PlutusTx
import           Language.PlutusTx.Prelude

import           Ledger.Constraints.UtxoLike
import           Ledger.Crypto               (PubKey)
import           Ledger.Interval             (isEmpty)
import           Ledger.Scripts              (DataValue (..), ValidatorHash)
import           Ledger.Slot                 (SlotRange)
import           Ledger.Value                (Value, isZero)

import qualified Prelude                     as Haskell

-- | Restrictions placed on the allocation of funds to outputs of transactions.
data TxConstraints i o =
    TxConstraints
        { tcOutputs            :: o
        -- ^ Constraints on the outputs of the transaction
        , tcInputs             :: i
        -- ^ Constraints on the inputs of the transaction
        , tcDataValues         :: [DataValue]
        -- ^ Data values to be included in the spending transaction
        , tcForge              :: Value
        -- ^ How much value is to be forged
        , tcInterval           :: SlotRange
        -- ^ Validity interval of this 'TxConstraints'
        , tcRequiredSignatures :: [PubKey]
        -- ^ Signatories of the transaction
        , tcValueSpent         :: Value
        -- ^ The minimum value that needs to be spent by the transaction. The
        --   purpose of this field is to enable proof of ownership for tokens
        --   (a transaction proves ownership of a token if the value consumed
        --   and spent by it includes the token. The value in the 'tcValueSpent'
        --   field will be paid from the wallet's own funds back to an address
        --   owned by the wallet)
        } deriving stock (Haskell.Functor, Haskell.Eq, Haskell.Show, Generic)
          deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, IotsType)

instance Bifunctor TxConstraints where
    bimap f g txc =
        txc { tcInputs = f (tcInputs txc)
            , tcOutputs = g (tcOutputs txc)
            }

instance (Semigroup i, Semigroup o) => Semigroup (TxConstraints i o) where
    {-# INLINABLE (<>) #-}
    l <> r =
        TxConstraints
            { tcOutputs = tcOutputs l <> tcOutputs r
            , tcDataValues = tcDataValues l <> tcDataValues r
            , tcForge = tcForge l <> tcForge r
            , tcInterval = tcInterval l /\ tcInterval r
            , tcRequiredSignatures = tcRequiredSignatures l <> tcRequiredSignatures r
            , tcValueSpent = tcValueSpent l <> tcValueSpent r
            , tcInputs = tcInputs l <> tcInputs r
            }

instance (Haskell.Semigroup i, Haskell.Semigroup o) => Haskell.Semigroup (TxConstraints i o) where
    l <> r =
        TxConstraints
            { tcOutputs = tcOutputs l Haskell.<> tcOutputs r
            , tcDataValues = tcDataValues l Haskell.<> tcDataValues r
            , tcForge = tcForge l Haskell.<> tcForge r
            , tcInterval = tcInterval l /\ tcInterval r
            , tcRequiredSignatures = tcRequiredSignatures l Haskell.<> tcRequiredSignatures r
            , tcValueSpent = tcValueSpent l Haskell.<> tcValueSpent r
            , tcInputs = tcInputs l Haskell.<> tcInputs r
            }

instance (Monoid i, Monoid o) => Monoid (TxConstraints i o) where
    {-# INLINABLE mempty #-}
    mempty  = TxConstraints mempty mempty mempty mempty top mempty mempty

instance (Haskell.Monoid i, Haskell.Monoid o) => Haskell.Monoid (TxConstraints i o) where
    mappend = (Haskell.<>)
    mempty  = TxConstraints Haskell.mempty Haskell.mempty Haskell.mempty Haskell.mempty top Haskell.mempty Haskell.mempty

instance (Eq o, Eq i) => Eq (TxConstraints i o) where
    {-# INLINABLE (==) #-}
    l == r =
        tcOutputs l == tcOutputs r
        && tcInputs l == tcInputs r
        && tcDataValues l == tcDataValues r
        && tcForge l == tcForge r
        && tcInterval l == tcInterval r
        && tcRequiredSignatures l == tcRequiredSignatures r
        && tcValueSpent l == tcValueSpent r

instance (Pretty i, Pretty o) => Pretty (TxConstraints [i] [o]) where
    pretty TxConstraints{tcOutputs, tcDataValues, tcForge, tcInterval, tcRequiredSignatures, tcValueSpent, tcInputs} =
        vsep
            [ hang 2 (vsep ("inputs:" : fmap pretty tcInputs))
            , hang 2 (vsep ("outputs:" : fmap pretty tcOutputs))
            , hang 2 (vsep ("data values:" : fmap pretty tcDataValues))
            , hang 2 (vsep ["value forged:", pretty tcForge])
            , hang 2 (vsep ["validity range:", viaShow tcInterval])
            , hang 2 (vsep ("required signatures:" : fmap pretty tcRequiredSignatures))
            , hang 2 (vsep ["value moved:", pretty tcValueSpent])
            ]

{-# INLINABLE validIn #-}
-- | @validIn r@ requires the transaction's slot range to be contained
--   in @r@.
validIn :: (Monoid i, Monoid o) => SlotRange -> TxConstraints i o
validIn range = mempty { tcInterval = range }

{-# INLINABLE signedBy #-}
-- | Require the transaction to be signed by the public key.
signedBy :: (Monoid i, Monoid o) => PubKey -> TxConstraints i o
signedBy pk = mempty { tcRequiredSignatures = [pk] }

{-# INLINABLE produceOutput #-}
-- | Require the transaction to produce the ouptut
produceOutput :: forall i o. Monoid i => o -> TxConstraints i [o]
produceOutput o = (mempty @(TxConstraints i [o])) { tcOutputs = [o] }

{-# INLINABLE includeDataValue #-}
-- | Require the transaction to include a data value
includeDataValue :: (Monoid i, Monoid o) => DataValue -> TxConstraints i o
includeDataValue dv = mempty { tcDataValues = [dv] }

{-# INLINABLE payToScript #-}
-- | Lock the value with a script
payToScript :: forall i o. (Monoid i, UtxoLike o) => Value -> ValidatorHash -> DataValue -> TxConstraints i [o]
payToScript v a ds =
    let outp = scriptUtxo a ds v
    in (mempty @(TxConstraints i [o]))
        { tcOutputs = [outp]
        , tcDataValues = [ds]
        }

{-# INLINABLE payToPubKey #-}
-- | Lock the value with a public key
payToPubKey :: forall i o. (Monoid i, UtxoLike o) => Value -> PubKey -> TxConstraints i [o]
payToPubKey v pk = (mempty @(TxConstraints i [o])) { tcOutputs = [pubKeyUtxo pk v] }

-- This just sets the 'tcForge' field, but it's exported for convenient
-- use with '<>'.
-- | Create the given value
forgeValue :: forall i o. (Monoid i, Monoid o) => Value -> TxConstraints i o
forgeValue vl = (mempty @(TxConstraints i o)) { tcForge = vl }

-- | Requirement to spend the given value
spendValue :: forall i o. (Monoid i, Monoid o) => Value -> TxConstraints i o
spendValue vl = (mempty @(TxConstraints i o)) { tcValueSpent = vl }

{-# INLINABLE isSatisfiable #-}
-- | Is there a valid transaction that satisfies the constraints? (ignoring
--   the inputs and outputs)
isSatisfiable :: TxConstraints i o -> Bool
isSatisfiable TxConstraints{tcInterval} = not (isEmpty tcInterval)

-- | Can the constraints be satisfied by a transaction with no
--   inputs and outputs?
modifiesUtxoSet :: TxConstraints [i] [o] -> Bool
modifiesUtxoSet TxConstraints{tcForge, tcInputs, tcOutputs, tcValueSpent} =
    not (isZero tcForge)
    || not (null tcInputs)
    || not (null tcOutputs)
    || not (isZero tcValueSpent)

PlutusTx.makeIsData ''TxConstraints
PlutusTx.makeLift ''TxConstraints

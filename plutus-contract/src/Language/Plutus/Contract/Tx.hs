{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Language.Plutus.Contract.Tx(
    TxConstraints
    , PendingTxConstraints
    , LedgerTxConstraints
    , collectFromScript
    , collectFromScriptFilter
    , payToScript
    , payToPubKey
    , forgeValue
    , spendValue
    , validIn
    , signedBy
    , spendInput
    , produceOutput
    , payToOwnAddress
    , includeDataValue
    , modifiesUtxoSet
    , hasValidTx
    -- * Constructing inputs
    , Tx.pubKeyTxIn
    , Tx.scriptTxIn
    , Tx.TxOutRef(..)
    -- * Constructing outputs
    , Tx.pubKeyTxOut
    , Tx.scriptTxOut
    , Tx.scriptTxOut'
    ) where

import           Ledger             (RedeemerValue, TxOutRef, TxOutTx, Validator)
import           Ledger.AddressMap  (AddressMap)
import           Ledger.Constraints
import qualified Ledger.Tx          as Tx

import qualified Wallet.API         as WAPI

-- | A set of constraints for a transaction that collects script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs. See 'Wallet.API.collectFromScript'.
collectFromScript
    :: AddressMap
    -> Validator
    -> RedeemerValue
    -> LedgerTxConstraints
collectFromScript = collectFromScriptFilter (\_ -> const True)

-- | See 'Wallet.API.collectFromScriptFilter'.
collectFromScriptFilter
    :: (TxOutRef -> TxOutTx -> Bool)
    -> AddressMap
    -> Validator
    -> RedeemerValue
    -> LedgerTxConstraints
collectFromScriptFilter flt am vls red =
    let inp = WAPI.getScriptInputsFilter flt am vls red
    in foldMap spendInput (fmap fst inp)

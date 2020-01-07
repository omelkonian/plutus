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
    , moveValue
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

{- Note [Unbalanced transactions] -- FIXME update

To turn an 'LedgerTxConstraints' into a valid transaction that can be submitted to the
network, the contract backend needs to

* Balance it.
  If the total value of `utxInputs` + the `txForge` field is
  greater than the total value of `utxOutput`, then one or more public key
  outputs need to be added. How many and what addresses they are is up
  to the wallet (probably configurable).
  If the total balance `utxInputs` + the `txForge` field is less than
  the total value of `utxOutput`, then one or more public key inputs need
  to be added (and potentially some outputs for the change).

* Compute fees.
  Once the final size of the transaction is known, the fees for the transaction
  can be computed. The transaction fee needs to be paid for with additional
  inputs so I assume that this step and the previous step will be combined.

  Also note that even if the 'LedgerTxConstraints' that we get from the contract
  endpoint happens to be balanced already, we still need to add fees to it. So
  we can't skip the balancing & fee computation step.

* Sign it.
  The signing process needs to provide signatures for all public key
  inputs in the balanced transaction, and for all public keys in the
  `utxRequiredSignatures` field.

While there is an 'empty' transaction we can't make 'LedgerTxConstraints' a monoid
because it's not clear what the binary operator should do with the validity
interval. There are two valid options: Hull and intersection. ('always' is the
unit for the intersection but then there is the issue that we don't have a
canonical representation of the empty interval (that's why 'intersection'
returns a 'Maybe Interval'.))

-}

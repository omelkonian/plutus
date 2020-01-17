{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeApplications   #-}
module Ledger.Constraints.OffChain where

import qualified Data.Map                         as Map
import qualified Data.Set                         as Set

import           Language.PlutusTx                (IsData (..))
import qualified Language.PlutusTx                as PlutusTx

import           Ledger.Address                   (Address, scriptHashAddress)
import           Ledger.Constraints.OnChain
import           Ledger.Constraints.TxConstraints
import           Ledger.Scripts                   (DataValue (..))
import           Ledger.Tx                        (Tx (..))
import qualified Ledger.Tx                        as LTx

-- | Constraints on a transaction in off-chain code.
type LedgerTxConstraints  = TxConstraints [LTx.TxIn] [LTx.TxOut]

{-# INLINABLE spendInput #-}
-- | Require the transaction to spend the input
spendInput :: forall o. Monoid o => LTx.TxIn -> TxConstraints [LTx.TxIn] o
spendInput i = (mempty @(TxConstraints [LTx.TxIn] o)) { tcInputs = [i] }

-- | The tightest set of constraints that are satisfied by the given ledger 
--   transaction
fromLedgerTx :: Tx -> LedgerTxConstraints
fromLedgerTx Tx{txInputs, txOutputs, txForge, txValidRange, txSignatures, txData} =
    TxConstraints
        { tcInputs = Set.toList txInputs
        , tcOutputs = txOutputs
        , tcForge = txForge
        , tcInterval = txValidRange
        , tcRequiredSignatures = Set.toList (Map.keysSet txSignatures)
        , tcDataValues = snd <$> Map.toList txData
        , tcValueSpent = foldMap LTx.txOutValue txOutputs
        }

fromOnChainUtxo :: IsData a => Address -> OnChainUtxo a -> LTx.TxOut
fromOnChainUtxo ownAddress = \case
    OnChainPayToPubKey pk vl -> LTx.pubKeyTxOut vl pk
    OnChainPayToScript vh dv vl -> LTx.scriptTxOut' vl (scriptHashAddress vh) dv
    OnChainPayToSelf dv vl -> LTx.scriptTxOut' vl ownAddress (DataValue $ toData dv)

-- | Turn an on-chain 'PendingTxConstraints' value into an (off-chain)
--   'LedgerTxConstraints' value, using the validator
--   for the output at the 'PendingTxConstraints'' "own address"
toLedgerConstraints :: PlutusTx.IsData a => Address -> PendingTxConstraints a -> LedgerTxConstraints
toLedgerConstraints addr txc =
    txc { tcInputs = []
        , tcOutputs = fmap (fromOnChainUtxo addr) (tcOutputs txc)
        }

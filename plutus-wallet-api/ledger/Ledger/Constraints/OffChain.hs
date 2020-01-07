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
import           Ledger.Scripts                   (DataValue (..), dataValueHash)
import           Ledger.Tx                        (Tx (..))
import qualified Ledger.Tx                        as LTx

-- | Constraints on a transaction in off-chain code.
type LedgerTxConstraints  = TxConstraints [LTx.TxIn] [LTx.TxOut]

{-# INLINABLE spendInput #-}
-- | Require the transaction to spend the input
spendInput :: LTx.TxIn -> LedgerTxConstraints -- applicative f
spendInput i = (mempty @LedgerTxConstraints) { tcInputs = [i] }

-- | A ledger transaction that satisfies the constraints (unbalanced and
--   unsigned)
toLedgerTx :: LedgerTxConstraints -> Tx
toLedgerTx TxConstraints{tcInputs, tcOutputs, tcForge, tcInterval, tcDataValues} =
    Tx
        { txInputs = Set.fromList tcInputs
        , txOutputs = tcOutputs
        , txForge = tcForge
        , txFee = mempty
        , txValidRange = tcInterval
        , txSignatures = Map.empty
        , txData = Map.fromList $ fmap (\ds -> (dataValueHash ds, ds)) tcDataValues
        }

-- | Constraints that are satisfied by the given ledger transaction
fromLedgerTx :: Tx -> LedgerTxConstraints
fromLedgerTx Tx{txInputs, txOutputs, txForge, txValidRange, txSignatures, txData} =
    TxConstraints
        { tcInputs = Set.toList txInputs
        , tcOutputs = txOutputs
        , tcForge = txForge
        , tcInterval = txValidRange
        , tcRequiredSignatures = Set.toList (Map.keysSet txSignatures)
        , tcDataValues = snd <$> Map.toList txData
        , tcValueMoved = mempty
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


{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Ledger.Constraints.UtxoLike where

import           Ledger.Address (scriptHashAddress)
import           Ledger.Crypto  (PubKey (..))
import           Ledger.Scripts (DataValue, ValidatorHash)
import           Ledger.Tx      (TxOut)
import qualified Ledger.Tx      as LTx
import           Ledger.Value   (Value)

-- | A final encoding of unspent transaction outputs
class UtxoLike o where
  -- | A transaction output locked by a public key
  pubKeyUtxo :: PubKey -> Value -> o

  -- | A transaction output locked by a validator and a data value
  scriptUtxo :: ValidatorHash -> DataValue -> Value -> o

instance UtxoLike TxOut where
  pubKeyUtxo = flip LTx.pubKeyTxOut
  scriptUtxo vh dv vl = LTx.scriptTxOut' vl (scriptHashAddress vh) dv

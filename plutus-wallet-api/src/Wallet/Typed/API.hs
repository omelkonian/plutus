{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Wallet.Typed.API where

import qualified Language.PlutusTx        as PlutusTx
import           Ledger.AddressMap
import           Ledger.Tx
import qualified Ledger.Typed.Constraints as Typed
import qualified Ledger.Typed.Scripts     as Scripts
import qualified Ledger.Typed.Tx          as Typed

import           Control.Lens

import           Data.Either
import qualified Data.Map                 as Map
import           Data.Maybe

-- | Given the pay to script address of the 'Validator', collect from it
--   all the outputs that match a predicate, using the 'RedeemerValue'.
collectFromScriptFilter ::
    forall a
    . (PlutusTx.IsData (Scripts.DataType a), PlutusTx.IsData (Scripts.RedeemerType a))
    => (TxOutRef -> TxOutTx -> Bool)
    -> AddressMap
    -> Scripts.ScriptInstance a
    -> Scripts.RedeemerType a
    -> Typed.TypedTxSomeIns '[]
collectFromScriptFilter flt am si red =
    let adr     = Scripts.scriptAddress si
        utxo :: Map.Map TxOutRef TxOutTx
        utxo    = fromMaybe Map.empty $ am ^. at adr
        ourUtxo :: Map.Map TxOutRef TxOutTx
        ourUtxo = Map.filterWithKey flt utxo
        -- We just throw away any outputs at this script address that don't typecheck.
        -- TODO: we should log this, it would make debugging much easier
        typedRefs :: [Typed.TypedScriptTxOutRef a]
        typedRefs = rights $ Typed.typeScriptTxOutRef @a (\ref -> Map.lookup ref utxo) si <$> Map.keys ourUtxo
        typedIns :: [Typed.TypedScriptTxIn a]
        typedIns = Typed.makeTypedScriptTxIn @a si red <$> typedRefs
    -- We need to add many txins and we've done as much checking as we care to, so we switch to TypedTxSomeIns
    in Typed.addManyTypedTxIns typedIns Typed.emptyTypedTxConstraints

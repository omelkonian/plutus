{-# LANGUAGE FlexibleContexts #-}
module LetFloating where

import qualified Language.PlutusCore.Name                as PLC

import Language.PlutusIR

-- | todo
rearrangeLets
    :: (PLC.HasUnique (name a) PLC.TermUnique, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Term tyname name a
    -> Term tyname name a
rearrangeLets = undefined

data LetGroup = LetGroup {
                   lambdaDepend :: PLC.Unique,
                   introducedVars :: [PLC.Unique]
                }

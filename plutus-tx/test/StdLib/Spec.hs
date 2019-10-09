{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# OPTIONS -fplugin Language.PlutusTx.Plugin -fplugin-opt Language.PlutusTx.Plugin:defer-errors -fplugin-opt Language.PlutusTx.Plugin:no-context #-}
module StdLib.Spec where

import           Common
import           PlcTestUtils

import qualified Language.PlutusTx.Ratio as Ratio

import           Language.PlutusTx.Code
import qualified Language.PlutusTx.Lift     as Lift
import           Language.PlutusTx.Plugin

roundPlc :: CompiledCode (Rational -> Integer)
roundPlc = plc @"roundPlc" Ratio.round

tests :: TestNested
tests = 
  testNested "stdlib"
    [ goldenEval "ratioInterop" [ getPlc roundPlc, Lift.liftProgram (3.75::Rational) ]
    ]

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
module Language.PlutusTx.Ratio(
    Ratio(..)
    , numerator
    , denominator
    , round
    , truncate
    , properFraction
    , half
    -- * Misc.
    , quotRem
    , gcd
    ) where

import qualified Language.PlutusTx.Applicative as P
import qualified Language.PlutusTx.IsData      as P
import qualified Language.PlutusTx.Data        as P
import qualified Language.PlutusTx.Numeric     as P
import qualified Language.PlutusTx.Eq          as P
import qualified Language.PlutusTx.Lift        as P
import qualified Language.PlutusTx.Ord         as P
import qualified Language.PlutusTx.Bool        as P

import qualified Language.PlutusTx.Builtins as Builtins

import Language.PlutusTx.Functor

import GHC.Real (Ratio(..))
import Prelude (Bool, Integer, Maybe(..))

instance P.IsData a => P.IsData (Ratio a) where
    {-# INLINABLE toData #-}
    toData (n :% d) = P.Constr 0 [P.toData n, P.toData d]
    {-# INLINABLE fromData #-}
    fromData (P.Constr i [n, d]) | i P.== 0 = (:%) <$> P.fromData n P.<*> P.fromData d
    fromData _ = Nothing

instance P.Eq a => P.Eq (Ratio a) where
    {-# INLINABLE (==) #-}
    (n1 :% d1) == (n2 :% d2) = n1 P.== n2 P.&& d1 P.== d2

instance P.AdditiveSemigroup (Ratio Integer) where
    {-# INLINABLE (+) #-}
    (x :% y) + (x' :% y') = ((x P.* y') P.+ (x' P.* y)) :% (y P.* y')

instance P.AdditiveMonoid (Ratio Integer) where
    {-# INLINABLE zero #-}
    zero = P.zero :% P.one

instance P.AdditiveGroup (Ratio Integer) where
    {-# INLINABLE (-) #-}
    (x :% y) - (x' :% y') = ((x P.* y') P.- (x' P.* y)) :% (y P.* y')

instance P.MultiplicativeSemigroup (Ratio Integer) where
    {-# INLINABLE (*) #-}
    (x :% y) * (x' :% y') = (x P.* x') :% (y P.* y')

instance P.Ord (Ratio Integer) where
    {-# INLINABLE (<=) #-}
    (x :% y) <= (x' :% y') = x P.* y' P.<= (x' P.* y)

P.makeLift ''Ratio

numerator :: Ratio a -> a
numerator (n :% _) = n

denominator :: Ratio a -> a
denominator (_ :% d) = d

-- From GHC.Real
-- | @'gcd' x y@ is the non-negative factor of both @x@ and @y@ of which
-- every common factor of @x@ and @y@ is also a factor; for example
-- @'gcd' 4 2 = 2@, @'gcd' (-4) 6 = 2@, @'gcd' 0 4@ = @4@. @'gcd' 0 0@ = @0@.
gcd :: Integer -> Integer -> Integer
gcd a 0  =  a
gcd a b  =  gcd b (a `Builtins.remainderInteger` b)

-- | 'reduce' is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
reduce :: Integer -> Integer -> Ratio Integer
reduce _ 0 =  Builtins.error ()
reduce x y =  (x `Builtins.divideInteger` d) :% (y `Builtins.divideInteger` d) where d = gcd x y

-- | truncate @x@ returns the integer nearest @x@ between zero and @x@
truncate :: Ratio Integer -> Integer
truncate (n :% d) = n `Builtins.divideInteger` d

-- From GHC.Real
-- | The function 'properFraction' takes a real fractional number @x@
-- and returns a pair @(n,f)@ such that @x = n+f@, and:
--
-- * @n@ is an integral number with the same sign as @x@; and
--
-- * @f@ is a fraction with the same type and sign as @x@,
--   and with absolute value less than @1@.
--
-- The default definitions of the 'ceiling', 'floor', 'truncate'
-- and 'round' functions are in terms of 'properFraction'.
properFraction :: Ratio Integer -> (Integer, Ratio Integer)
properFraction (n :% d) = (q, r :% d) where (q, r) = quotRem n d

quotRem :: Integer -> Integer -> (Integer, Integer)
quotRem x y = (x `Builtins.divideInteger` y, x `Builtins.remainderInteger` y)
  -- no quotRem builtin :(

half :: Ratio Integer
half = 1 :% 2

absR :: Ratio Integer -> Ratio Integer
absR (n :% d) = (abs n :% abs d)

abs x = if x P.< P.zero then (P.negate x) else x

signum :: Ratio Integer -> Integer
signum (0 :% _) = 0
signum d = if d P.> P.zero then 1 else -1

even :: Integer -> Bool
even x = (x `Builtins.remainderInteger` 2) P.== P.zero

round :: Ratio Integer -> Integer
round x =
  let (n, r) = properFraction x
      m      = if r P.< P.zero then n P.- P.one else n P.+ P.one
  in case signum (abs r P.- half) of
      -1 -> n
      0  -> if even n then n else m
      1  -> m
      _  -> Builtins.error ()

lcm :: Integer -> Integer -> Integer
lcm _ 0         =  0
lcm 0 _         =  0
lcm x y         =  abs ((x `Builtins.divideInteger` (gcd x y)) P.* y)

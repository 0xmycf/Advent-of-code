--------------------------------------------------------------------------------
-- |
-- Module      :  Finite
-- Copyright   :  (C) 2015-2022 mniip and (C) 2022-2023 0xmycf for any made changes
-- License     :  BSD3
-- Stability   :  experimental
-- Source      :  https://hackage.haskell.org/package/finite-typelits-0.1.6.0/docs/Data-Finite.html
--------------------------------------------------------------------------------
module Finite (Finite, finite, unwrap) where
import           GHC.TypeLits (KnownNat, natVal)
import           GHC.TypeNats (Nat)

-- | Finite number range for Int(egers)
-- Ex. Finite 2 <=> {0 , 1} ...
newtype Finite (a :: Nat) = Finite Integer
  deriving stock (Show, Eq, Ord)

finite :: KnownNat n => Integer -> Finite n
finite x = result
    where
    result = if x < natVal result && x >= 0
              then Finite x else error $ show x ++ " Out of bounds for " ++ show (natVal result)

-- | Convert a 'Finite' into the corresponding 'Integer'.
unwrap :: Finite n -> Integer
unwrap (Finite x) = x

lift :: Finite a -> (Integer -> Integer) -> Finite a
lift f1 fun = Finite . fun . unwrap $ f1

-- | +, -, * use modulo
-- abs is the identity
-- signum can only return 0 or 1 by definition
-- fromInteger is 'Finite.finite'
-- negate throws an error if the number is not 0
instance KnownNat a => Num (Finite a) where
  (+) f1 f2 = Finite ((unwrap f1 + unwrap f2) `mod` natVal f1)
  (*) f1 f2 = Finite ((unwrap f1 * unwrap f2) `mod` natVal f1)
  (-) f1 f2 = Finite ((unwrap f1 - unwrap f2) `mod` natVal f1)
  -- id because its a finite collection of natural numbers and forall a in N : a >= 0
  abs = id
  -- signum can only be 0 or 1
  signum = flip lift signum
  fromInteger = finite
  -- | Cannot negate a natural number
  negate f = if signum f > 0
              then error $ "Cannot negate positive number since, the inverse (+) would be out of bounds ("
                            ++ (show . negate . unwrap)f
                            ++ " < "
                            ++ show (natVal f)
                            ++ ")"
              else 0


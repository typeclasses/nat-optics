{-# language Trustworthy #-}

module NatOptics.Positive.Math
  (
    minus,
    plus,
    absoluteDifference,
  )
  where

import Data.Ord            ( Ord, (<), compare, Ordering (..) )
import Prelude             ( Num, (+), (-) )

import NatOptics.Positive.Unsafe ( Positive (..) )

import NatOptics.Signed ( Signed )
import qualified NatOptics.Signed as Signed

import NatOptics.NonNegative.Unsafe ( NonNegative (..) )

plus :: Num n => Positive n -> Positive n -> Positive n
plus (PositiveUnsafe a) (PositiveUnsafe b) = PositiveUnsafe (a + b)

minus :: (Num n, Ord n) => Positive n -> Positive n -> Signed n
minus (PositiveUnsafe a) (PositiveUnsafe b) =
    case compare a b of
        EQ -> Signed.Zero
        GT -> Signed.Plus (PositiveUnsafe (a - b))
        LT -> Signed.Minus (PositiveUnsafe (b - a))

absoluteDifference :: (Num n, Ord n) => Positive n -> Positive n -> NonNegative n
absoluteDifference (PositiveUnsafe a) (PositiveUnsafe b) =
    NonNegativeUnsafe (if a < b then b - a else a - b)

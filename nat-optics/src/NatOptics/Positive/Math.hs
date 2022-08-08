{-# language Trustworthy #-}

module NatOptics.Positive.Math
  (
    minus,
    plus,
  )
  where

import Data.Ord            ( Ord, compare, Ordering (..) )
import Prelude             ( Num, (+), (-) )

import NatOptics.Positive.Unsafe ( Positive (..) )

import NatOptics.Signed ( Signed )
import qualified NatOptics.Signed as Signed

plus :: Num n => Positive n -> Positive n -> Positive n
plus (PositiveUnsafe a) (PositiveUnsafe b) = PositiveUnsafe (a + b)

minus :: (Num n, Ord n) => Positive n -> Positive n -> Signed n
minus (PositiveUnsafe a) (PositiveUnsafe b) =
    case compare a b of
        EQ -> Signed.Zero
        GT -> Signed.Plus (PositiveUnsafe (a - b))
        LT -> Signed.Minus (PositiveUnsafe (b - a))

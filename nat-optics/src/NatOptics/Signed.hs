{-# language Trustworthy #-}

module NatOptics.Signed
  (
    Signed (..),
    intIso,
    intNatIso,
  )
  where

import Data.Eq            ( Eq )
import Data.Ord           ( Ord, compare, Ordering (..) )
import Numeric.Natural    ( Natural )
import Optics.Core        ( Iso', iso )
import Prelude            ( Integer, abs, negate, fromIntegral )
import Text.Show          ( Show )

import NatOptics.Positive.Unsafe ( Positive (..) )

data Signed n = Zero | Minus (Positive n) | Plus (Positive n)
    deriving stock (Eq, Ord, Show)

intIso :: Iso' Integer (Signed Integer)
intIso = iso f g
  where
    f x = case compare x 0 of
        EQ -> Zero
        LT -> Minus (PositiveUnsafe (abs x))
        GT -> Plus (PositiveUnsafe x)
    g y = case y of
        Zero -> 0
        Plus (PositiveUnsafe x) -> x
        Minus (PositiveUnsafe x) -> negate x

intNatIso :: Iso' Integer (Signed Natural)
intNatIso = iso f g
  where
    f x = case compare x 0 of
        EQ -> Zero
        LT -> Minus (PositiveUnsafe (fromIntegral (abs x)))
        GT -> Plus (PositiveUnsafe (fromIntegral x))
    g y = case y of
        Zero -> 0
        Plus (PositiveUnsafe x) -> fromIntegral x
        Minus (PositiveUnsafe x) -> negate (fromIntegral x)

{-# language Trustworthy #-}

module NatOptics.Signed
  (
    Signed (..),
    intIso,
  )
  where

import Data.Eq            ( Eq )
import Data.Ord           ( Ord, compare, Ordering (..) )
import Text.Show          ( Show )
import Optics.Core        ( Iso', iso )
import Prelude            ( Integer, abs, negate )

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

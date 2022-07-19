module NatOptics.Signed
  (
    Signed (..),
  )
  where

import Data.Eq            ( Eq )
import Data.Ord           ( Ord )
import Text.Show          ( Show )

import NatOptics.Positive ( Positive )

data Signed n = Zero | Minus (Positive n) | Plus (Positive n)
    deriving stock (Eq, Ord, Show)

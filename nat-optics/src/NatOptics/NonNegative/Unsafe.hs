{-# language Unsafe #-}

module NatOptics.NonNegative.Unsafe
  (
    NonNegative (..),
  )
  where

import Data.Eq   ( Eq )
import Data.Ord  ( Ord )
import Text.Show ( Show )

newtype NonNegative number = NonNegativeUnsafe{ number :: number }
    deriving newtype (Eq, Ord, Show)

{-# language Unsafe #-}

module NatOptics.Positive.Unsafe where

import Data.Eq   ( Eq )
import Data.Ord  ( Ord )
import Text.Show ( Show )

newtype Positive number = PositiveUnsafe{ number :: number }
    deriving newtype (Eq, Ord, Show)

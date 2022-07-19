{-# language Trustworthy #-}

module NatOptics.Math
  (
    positiveMinus,
  )
  where

import Data.Maybe          ( Maybe (..) )
import Data.Ord            ( Ord, (<) )
import Optics.Core         ( preview, review )
import Prelude             ( Num, abs, (-) )

import NatOptics.Positive ( Positive )
import qualified NatOptics.Positive as Positive

import NatOptics.Signed ( Signed )
import qualified NatOptics.Signed as Signed

positiveMinus :: (Num n, Ord n) => Positive n -> Positive n -> Signed n
positiveMinus a b =
    let
        a' = review Positive.refine a
        b' = review Positive.refine b
        diff = a' - b'
    in
        case preview Positive.refine (abs diff) of
            Nothing -> Signed.Zero
            Just diff' -> if diff < 0 then Signed.Minus diff' else Signed.Plus diff'

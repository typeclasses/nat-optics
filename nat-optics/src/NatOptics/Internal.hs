{-# language Trustworthy #-}

module NatOptics.Internal
  (
    strNat, textStr,
  )
  where

import Control.Monad   ( mfilter )
import Data.Eq         ( (==) )
import Data.String     ( String )
import Data.Text       ( Text )
import Numeric.Natural ( Natural )
import Optics.Iso      ( Iso', iso )
import Optics.Prism    ( Prism', prism' )
import Text.Read       ( Read, readMaybe )
import Text.Show       ( Show, show )

import qualified Data.Text as Text

textStr :: Iso' Text String
textStr = iso Text.unpack Text.pack

strNat :: Prism' String Natural
strNat = readShowPrism

readShowPrism :: (Read a, Show a) => Prism' String a
readShowPrism = prism' show (\str -> mfilter (\n -> show n == str) (readMaybe str))

module NatOptics.Positive
  (
    {- * Type constructor -} Positive,

    {- * Optics -}           refine, natPrism, intPrism,
                             textPrism, stringPrism,

    {- * Re-exports -}       Natural, Integer, Prism',
                             view, review, preview
  ) where

import Control.Applicative ( (*>) )
import Control.Monad       ( guard )
import Data.Bits           ( Bits, toIntegralSized )
import Data.Eq             ( Eq )
import Data.Function       ( (.) )
import Data.Functor        ( ($>), (<$>) )
import Data.Maybe          ( Maybe )
import Data.Ord            ( Ord, (>) )
import Data.String         ( String )
import Data.Text           ( Text )
import NatOptics.Internal  ( strNat, textStr )
import Numeric.Natural     ( Natural )
import Optics.AffineFold   ( preview )
import Optics.Getter       ( view )
import Optics.Optic        ( (%) )
import Optics.Prism        ( Prism', prism' )
import Optics.Review       ( review )
import Prelude             ( Integer, Integral, Num,
                             fromIntegral, toInteger )
import Text.Show           ( Show )

newtype Positive number = Positive{ number :: number }
    deriving newtype (Eq, Ord, Show)

{- | For any numeric type @n@,
     @'Positive' n@ is a subset of @n@.-}
refine :: (Num n, Ord n) => Prism' n (Positive n)
refine = prism' number verify

{- | For any integral type @n@,
     @'Positive' n@ is a subset of 'Natural'. -}
natPrism :: (Integral n, Bits n) => Prism' Natural (Positive n)
natPrism = prism' (fromIntegral . number) verifyAndResize

{- | For any integral type @n@,
     @'Positive' n@ is a subset of 'Integer'. -}
intPrism :: (Integral n, Bits n) => Prism' Integer (Positive n)
intPrism = prism' (toInteger . number) verifyAndResize

stringPrism :: (Integral n, Bits n) => Prism' String (Positive n)
stringPrism = strNat % natPrism

textPrism :: (Integral n, Bits n) => Prism' Text (Positive n)
textPrism = textStr % stringPrism

verify :: (Num n, Ord n) => n -> Maybe (Positive n)
verify n = guard (n > 0) $> Positive n

verifyAndResize :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe (Positive b)
verifyAndResize x = verify x *> (Positive <$> toIntegralSized x)

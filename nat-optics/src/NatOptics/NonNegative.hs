module NatOptics.NonNegative
  (
    {- * Type constructor -} NonNegative,

    {- * Optics -}           refine, natPrism, intPrism,
                             natIso, textPrism, stringPrism,

    {- * Re-exports -}       Natural, Integer, Prism', Iso',
                             view, review, preview,
  ) where

import Control.Applicative ( (*>) )
import Control.Monad       ( guard )
import Data.Bits           ( Bits, toIntegralSized )
import Data.Eq             ( Eq )
import Data.Function       ( (.) )
import Data.Functor        ( fmap, ($>), (<$>) )
import Data.Maybe          ( Maybe )
import Data.Ord            ( Ord, (>=) )
import Data.String         ( String )
import Data.Text           ( Text )
import NatOptics.Internal  ( strNat, textStr )
import Numeric.Natural     ( Natural )
import Optics.AffineFold   ( preview )
import Optics.Getter       ( view )
import Optics.Iso          ( Iso', iso )
import Optics.Optic        ( (%) )
import Optics.Prism        ( Prism', prism' )
import Optics.Review       ( review )
import Prelude             ( Integer, Integral, Num,
                             fromIntegral, toInteger )
import Text.Show           ( Show )

newtype NonNegative number = NonNegative{ number :: number }
    deriving newtype (Eq, Ord, Show)

{- | For any numeric type @n@,
     @'NonNegative' n@ is a subset of @n@.

Examples:

- @'preview' 'refine' (-1 :: 'Integer')@ = @'Nothing'@
- @'preview' 'refine' (0 :: 'Integer')@ = @'Just' (NonNegative 0)@
- @'preview' 'refine' (1 :: 'Integer')@ = @'Just' (NonNegative 1)@
- @'preview' 'refine' (2 :: 'Integer')@ = @'Just' (NonNegative 2)@
-}
refine :: (Num n, Ord n) => Prism' n (NonNegative n)
refine = prism' number verify

{- | For any integral type @n@,
     @'NonNegative' n@ is a subset of 'Natural'. -}
natPrism :: (Integral n, Bits n) => Prism' Natural (NonNegative n)
natPrism =
    prism'
        (fromIntegral . number)
        (fmap NonNegative . toIntegralSized) {- No need to verify
            here, because Natural is always non-negative. The only
            check here is when converting from 'Natural' to ensure
            that it does not overflow the max bound of 'n'. -}

{- | For any integral type @n@,
     @'NonNegative' n@ is a subset of 'Integer'. -}
intPrism :: (Integral n, Bits n) => Prism' Integer (NonNegative n)
intPrism = prism' (toInteger . number) verifyAndResize

{- | 'Natural' and @'NonNegative' 'Natural'@ are the same thing. -}
natIso :: Iso' Natural (NonNegative Natural)
natIso = iso NonNegative number

stringPrism :: (Integral n, Bits n) => Prism' String (NonNegative n)
stringPrism = strNat % natPrism

textPrism :: (Integral n, Bits n) => Prism' Text (NonNegative n)
textPrism = textStr % stringPrism

verify :: (Ord n, Num n) => n -> Maybe (NonNegative n)
verify n = guard (n >= 0) $> NonNegative n

verifyAndResize :: (Integral a, Integral b, Bits a, Bits b)
                => a -> Maybe (NonNegative b)
verifyAndResize x = verify x *> (NonNegative <$> toIntegralSized x)

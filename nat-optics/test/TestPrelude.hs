module TestPrelude (module X, Failure (..), main', show', okay, fail', (!)) where

import Control.Monad as X
import Data.Foldable as X
import Data.Int as X
import Optics.Optic as X
import Optics.Re as X
import Prelude as X
import System.Exit as X

data Failure = Failure String

main' :: [Failure] -> IO ()
main' failures =
    unless (null failures) $ do
        for_ failures $ \(Failure x) -> putStrLn ("🔥 " ++ x)
        exitFailure

show' :: Show a => a -> String
show' x = showsPrec 11 x ""

okay :: [Failure]
okay = []

fail' :: String -> [Failure]
fail' x = [Failure x]

(!) :: String -> String -> String
x ! y = x <> " " <> y

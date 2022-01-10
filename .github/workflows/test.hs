import Data.Maybe
import System.Environment
import System.Process

main =
  do
    ghc <- readGHC <$> getEnv "ghc"
    targets <- return ["all"]
    callProcess "cabal" $ ["build"] ++ targets ++ constraints ghc
    callProcess "cabal" $ ["test"] ++ targets ++ ["--enable-tests"] ++ constraints ghc

x .= Just y  = Just ("--constraint=" ++ x ++ "==" ++ y)
x .= Nothing = Nothing

data GHC = GHC_8_10 | GHC_9_0 | GHC_9_2

readGHC s = case s of
    "8.10" -> GHC_8_10
    "9.0"  -> GHC_9_0
    "9.2"  -> GHC_9_2

constraints ghc = catMaybes
    [ "base" .= case ghc of
          GHC_8_10 -> Just "4.14.*"
          GHC_9_0  -> Just "4.15.*"
          GHC_9_2  -> Just "4.16.*"
    , "optics-core" .= case ghc of
          GHC_8_10 -> Just "0.4"
          _        -> Nothing
    , "text" .= case ghc of
          GHC_8_10 -> Just "1.2.3.*"
          GHC_9_2  -> Just "2.0.*"
          _        -> Nothing
    ]

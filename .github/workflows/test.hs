import System.Environment
import System.Process

main =
  do
    ghc <- getEnv "ghc"
    callProcess "cabal" ("build" : "all" : constraints ghc)

x .= y =
    "--constraint=" ++ x ++ "==" ++ y

constraints ghc = case ghc of
    "8.10" ->
        [ "base"         .= "4.14.*"
        , "hedghog"      .= "1.0.4"
        , "text"         .= "1.2.3.*"
        , "optics-core"  .= "0.4"
        ]
    "9.0" ->
        [ "base"         .= "4.15.*"
        , "text"         .= "1.2.4.*"
        ]

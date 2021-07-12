module Main (main) where

import NatOptics.Positive
import TestPrelude

main :: IO ()
main =
  do
    okay <- checkParallel $$(discover)
    when (not okay) exitFailure

prop_examples :: Property
prop_examples = withTests 1 $ property $
  do
    evalMaybe (preview (stringPrism @Int16) "1") >>= \n ->
      do
        review refine      n === 1
        review stringPrism n === "1"

    evalMaybe (preview (stringPrism @Int16) "57") >>= \n ->
      do
        review refine      n === 57
        review stringPrism n === "57"

    traverse_
        (\x -> preview (stringPrism @Int16) x === Nothing )
        [ "0", "-0", "-1", "00", "𝟝𝟟", "57 ", "057"
        , "99999999999999999999999" ]

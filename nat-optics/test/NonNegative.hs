module Main (main) where

import NatOptics.NonNegative
import TestPrelude

main :: IO ()
main =
  do
    okay <- checkParallel $$(discover)
    when (not okay) exitFailure

prop_examples :: Property
prop_examples = withTests 1 $ property $
  do
    evalMaybe (preview (stringPrism @Int32) "0") >>= \n ->
      do
        review refine      n === 0
        review stringPrism n === "0"

    evalMaybe (preview (stringPrism @Int32) "57") >>= \n ->
      do
        review refine      n === 57
        review stringPrism n === "57"

    traverse_
        (\x -> preview (stringPrism @Int32) x === Nothing )
        [ "-0", "-1", "00", "𝟝𝟟", "57 ", "057"
        , "9999999999999999999999999999" ]

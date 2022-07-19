module Main (main) where

import Test.Hspec
import Prelude

import Data.Int (Int16, Int32)
import Data.Maybe (fromJust, isNothing)
import Optics.Core (preview, review)

import qualified NatOptics.Positive as Pos
import qualified NatOptics.NonNegative as NN

main :: IO ()
main = hspec $ do
    nonNegativeSpec
    positiveSpec

nonNegativeSpec :: SpecWith ()
nonNegativeSpec = describe "NonNegative" $ do
    describe "0" $ do
        let n = fromJust $ preview (NN.stringPrism @Int32) "0"
        specify "refine" $ review NN.refine n `shouldBe` 0
        specify "stringPrism" $ review NN.stringPrism n `shouldBe` "0"
    describe "57" $ do
        let n = fromJust $ preview (NN.stringPrism @Int32) "57"
        specify "refine" $ review NN.refine n `shouldBe` 57
        specify "stringPrism" $ review NN.stringPrism n `shouldBe` "57"
    describe "stringPrism" $ do
        let examples = [ "-0", "-1", "00", "𝟝𝟟", "57 ", "057", "9999999999999999999999999999" ]
        specify "failures" $ all (isNothing . preview (NN.stringPrism @Int32)) examples

positiveSpec :: SpecWith ()
positiveSpec = describe "Positive" $ do
    describe "1" $ do
        let n = fromJust $ preview (Pos.stringPrism @Int16) "1"
        specify "refine" $ review Pos.refine n `shouldBe` 1
        specify "stringPrism" $ review Pos.stringPrism n `shouldBe` "1"
    describe "57" $ do
        let n = fromJust $ preview (Pos.stringPrism @Int16) "57"
        specify "refine" $ review Pos.refine n `shouldBe` 57
        specify "stringPrism" $ review Pos.stringPrism n `shouldBe` "57"
    describe "stringPrism" $ do
        let examples = [ "0", "-0", "-1", "00", "𝟝𝟟", "57 ", "057", "99999999999999999999999" ]
        specify "failures" $ all (isNothing . preview (Pos.stringPrism @Int16)) examples

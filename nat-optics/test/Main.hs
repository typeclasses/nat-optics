module Main (main) where

import Test.Hspec
import Prelude

import Data.Int (Int16, Int32)
import Data.Maybe (isNothing)
import Numeric.Natural (Natural)
import Optics.Core (preview, review, view)

import qualified NatOptics.Positive as Pos
import qualified NatOptics.Positive.Unsafe as Pos
import qualified NatOptics.Positive.Math as Pos.Math
import qualified NatOptics.NonNegative as NN
import qualified NatOptics.NonNegative.Unsafe as NN
import qualified NatOptics.Signed as S

main :: IO ()
main = hspec $ do
    nonNegativeSpec
    positiveSpec
    signedSpec
    mathSpec

nonNegativeSpec :: SpecWith ()
nonNegativeSpec = describe "NonNegative" $ do
    describe "0" $ do
        specify "preview stringPrism" $ preview (NN.stringPrism @Int32) "0" `shouldBe` Just (NN.NonNegativeUnsafe 0)
        specify "review refine" $ review NN.refine (NN.NonNegativeUnsafe @Int32 0) `shouldBe` 0
        specify "review stringPrism" $ review NN.stringPrism (NN.NonNegativeUnsafe @Int32 0) `shouldBe` "0"
    describe "57" $ do
        specify "preview stringPrism" $ preview (NN.stringPrism @Int32) "57" `shouldBe` Just (NN.NonNegativeUnsafe 57)
        specify "refine" $ review NN.refine (NN.NonNegativeUnsafe @Int32 57) `shouldBe` 57
        specify "stringPrism" $ review NN.stringPrism (NN.NonNegativeUnsafe @Int32 57) `shouldBe` "57"
    describe "preview stringPrism" $ do
        let examples = [ "-0", "-1", "00", "𝟝𝟟", "57 ", "057", "9999999999999999999999999999" ]
        specify "failures" $ all (isNothing . preview (NN.stringPrism @Int32)) examples

positiveSpec :: SpecWith ()
positiveSpec = describe "Positive" $ do
    describe "1" $ do
        specify "preview stringPrism" $ preview (Pos.stringPrism @Int16) "1" `shouldBe` Just (Pos.PositiveUnsafe 1)
        specify "refine" $ review Pos.refine (Pos.PositiveUnsafe @Int16 1) `shouldBe` 1
        specify "stringPrism" $ review Pos.stringPrism (Pos.PositiveUnsafe @Int16 1) `shouldBe` "1"
    describe "57" $ do
        specify "preview stringPrism" $ preview (Pos.stringPrism @Int16) "57" `shouldBe` Just (Pos.PositiveUnsafe 57)
        specify "refine" $ review Pos.refine (Pos.PositiveUnsafe @Int16 57) `shouldBe` 57
        specify "stringPrism" $ review Pos.stringPrism (Pos.PositiveUnsafe @Int16 57) `shouldBe` "57"
    describe "stringPrism" $ do
        let examples = [ "0", "-0", "-1", "00", "𝟝𝟟", "57 ", "057", "99999999999999999999999" ]
        specify "failures" $ all (isNothing . preview (Pos.stringPrism @Int16)) examples

signedSpec :: SpecWith ()
signedSpec = describe "Signed" $ do
    describe "intIso" $ do
        describe "view" $ do
            specify "0" $ view S.intIso 0 `shouldBe` S.Zero
            specify "3" $ view S.intIso 3 `shouldBe` S.Plus (Pos.PositiveUnsafe 3)
            specify "-5" $ view S.intIso (-5) `shouldBe` S.Minus (Pos.PositiveUnsafe 5)
        describe "review" $ do
            specify "0" $ review S.intIso S.Zero `shouldBe` 0
            specify "3" $ review S.intIso (S.Plus (Pos.PositiveUnsafe 3)) `shouldBe` 3
            specify "-5" $ review S.intIso (S.Minus (Pos.PositiveUnsafe 5)) `shouldBe` (-5)
    describe "intNatIso" $ do
        describe "view" $ do
            specify "0" $ view S.intNatIso 0 `shouldBe` S.Zero
            specify "3" $ view S.intNatIso 3 `shouldBe` S.Plus (Pos.PositiveUnsafe 3)
            specify "-5" $ view S.intNatIso (-5) `shouldBe` S.Minus (Pos.PositiveUnsafe 5)
        describe "review" $ do
            specify "0" $ review S.intNatIso S.Zero `shouldBe` 0
            specify "3" $ review S.intNatIso (S.Plus (Pos.PositiveUnsafe 3)) `shouldBe` 3
            specify "-5" $ review S.intNatIso (S.Minus (Pos.PositiveUnsafe 5)) `shouldBe` (-5)

mathSpec :: SpecWith ()
mathSpec = describe "Positive math" $ do
    describe "plus" $ do
        specify "10 + 5" $ Pos.Math.plus (Pos.PositiveUnsafe 10) (Pos.PositiveUnsafe 5) `shouldBe` Pos.PositiveUnsafe @Int32 15
    describe "minus" $ do
        specify "3 - 3" $ Pos.Math.minus (Pos.PositiveUnsafe 3) (Pos.PositiveUnsafe 3) `shouldBe` S.Zero @Natural
        specify "3 - 5" $ Pos.Math.minus (Pos.PositiveUnsafe 3) (Pos.PositiveUnsafe 5) `shouldBe` S.Minus @Natural (Pos.PositiveUnsafe 2)
        specify "9 - 5" $ Pos.Math.minus (Pos.PositiveUnsafe 9) (Pos.PositiveUnsafe 5) `shouldBe` S.Plus @Natural (Pos.PositiveUnsafe 4)

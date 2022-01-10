module Main (main) where

import NatOptics.NonNegative
import TestPrelude

main :: IO ()
main = main' failures

failures :: [Failure]
failures =
    case preview (stringPrism @Int32) "0" of
        Nothing -> fail' "preview (stringPrism @Int32) \"0\" = Nothing"
        Just n ->
            case review refine n of
                0 -> okay
                result -> fail' $ "review refine" ! show' n ! "=" ! show result
            <>
            case review stringPrism n of
                "0" -> okay
                result -> fail' $ "review stringPrism" ! show' n ! "=" ! show result
    <>
    case preview (stringPrism @Int32) "57" of
        Nothing -> fail' "preview (stringPrism @Int32) \"57\" = Nothing"
        Just n ->
            case review refine n of
                57 -> okay
                result -> fail' $ "review refine" ! show' n ! "=" ! show result
            <>
            case review stringPrism n of
                "57" -> okay
                result -> fail' $ "review stringPrism" ! show' n ! "=" ! show result
    <>
    foldMap
        (\x -> case preview (stringPrism @Int32) x of
            Nothing -> okay
            result -> fail' $ "preview (stringPrism @Int32)" ! show' x ! "=" ! show result
        )
        [ "-0", "-1", "00", "𝟝𝟟", "57 ", "057"
        , "9999999999999999999999999999" ]

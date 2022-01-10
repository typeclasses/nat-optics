module Main (main) where

import NatOptics.Positive
import TestPrelude

main :: IO ()
main = main' failures

failures :: [Failure]
failures =
    case preview (stringPrism @Int16) "1" of
        Nothing -> fail' "preview (stringPrism @Int16) \"1\" = Nothing"
        Just n ->
            case review refine n of
                1 -> okay
                result -> fail' $ "review refine" ! show' n ! "=" ! show result
            <>
            case review stringPrism n of
                "1" -> okay
                result -> fail' $ "review stringPrism" ! show' n ! "=" ! show result
    <>
    case preview (stringPrism @Int16) "57" of
        Nothing -> fail' "preview (stringPrism @Int16) \"57\" = Nothing"
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
        (\x -> case preview (stringPrism @Int16) x of
            Nothing -> okay
            result -> fail' $ "preview (stringPrism @Int16)" ! show' x ! "=" ! show result
        )
        [ "0", "-0", "-1", "00", "𝟝𝟟", "57 ", "057"
        , "99999999999999999999999" ]

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MyLib
import System.Environment (lookupEnv, withArgs)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Runners.AntXML (antXMLRunner)

main :: IO ()
main = do
    testTree <- testSpec "MyLib tests" spec

    mXmlPath <- lookupEnv "TASTY_ANT_XML"
    let ingredients = maybe defaultIngredients (const [antXMLRunner]) mXmlPath
    withArgs (maybe [] (\path -> ["--xml=" ++ path]) mXmlPath) $
        defaultMainWithIngredients ingredients testTree

spec :: Spec
spec = do
    describe "sumTwo" $ do
        it "adds two numbers correctly" $ do
            sumTwo 1 2 `shouldBe` 3
            sumTwo 0 0 `shouldBe` 0
            sumTwo (-1) 1 `shouldBe` 0

        it "is commutative: x + y == y + x" $
            property $
                \x y -> sumTwo x y == sumTwo y (x :: Int)

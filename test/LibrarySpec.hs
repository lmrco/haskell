{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Library
import System.Environment (lookupEnv, withArgs)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Runners.AntXML (antXMLRunner)

main :: IO ()
main = do
    testTree <- testSpec "Library tests" spec
    -- Look for the TASTY_ANT_XML environment variable
    mXmlPath <- lookupEnv "TASTY_ANT_XML"
    let ingredients = maybe defaultIngredients (const [antXMLRunner]) mXmlPath
        args = maybe [] (\path -> ["--xml=" ++ path]) mXmlPath
    -- Run tests with XML output if TASTY_ANT_XML is set
    withArgs args $ defaultMainWithIngredients ingredients testTree

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

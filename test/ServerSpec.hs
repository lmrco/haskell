{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (throwIO, try)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Client (HttpException, HttpExceptionContent (StatusCodeException))
import Network.Wreq
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "HTTP API" $ do
    it "GET /health should return 'OK'" $ do
        result <-
            try $ get "http://localhost:8080/health" ::
                IO (Either HttpException (Response LBS.ByteString))
        case result of
            Right res -> do
                res ^. responseStatus . statusCode `shouldBe` 200
                res ^. responseBody `shouldBe` "OK"
            Left err -> throwIO err

    -- Test the sum endpoint with valid input
    it "POST /sum {x: 1, y: 1} should return {result: 2}" $ do
        let payload = object ["x" .= (1 :: Int), "y" .= (1 :: Int)]
        result <-
            try $
                postWith
                    (defaults & header "Content-Type" .~ ["application/json"])
                    "http://localhost:8080/sum"
                    payload ::
                IO (Either HttpException (Response LBS.ByteString))
        case result of
            Right res -> do
                res ^. responseStatus . statusCode `shouldBe` 200
                res ^. responseBody `shouldBe` "{\"result\":2}"
            Left err -> throwIO err

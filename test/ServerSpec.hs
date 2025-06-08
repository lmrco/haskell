{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (throwIO, try)
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "HTTP API" $ do
    it "GET /health should return 'OK'" $ do
        result <- try $ get "http://localhost:8080/health" :: IO (Either HttpException (Response LBS.ByteString))
        case result of
            Right res -> do
                res ^. responseStatus . statusCode `shouldBe` 200
                res ^. responseBody `shouldBe` "OK"
            Left err -> throwIO err
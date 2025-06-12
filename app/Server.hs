{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Cardano.Api (Quantity, quantityToLovelace)
import Data.Aeson (FromJSON, object, parseJSON, withObject, (.:), (.=))
import Network.HTTP.Types.Status (status400)
import Network.Wai.Handler.Warp (run)
import Web.Scotty

-- Define a data type for the request body
data SumRequest = SumRequest {x :: Int, y :: Int}
    deriving (Show)

instance FromJSON SumRequest where
    parseJSON = withObject "SumRequest" $ \v -> do
        x <- v .: "x"
        y <- v .: "y"
        return $ SumRequest x y

newtype QuantityRequest = QuantityRequest {qVal :: Int}
    deriving (Show)

instance FromJSON QuantityRequest where
    parseJSON = withObject "QuantityRequest" $ \v -> do
        quantity <- v .: "quantity"
        return $ QuantityRequest quantity

--newtype Quantity = Quantity Int
--  deriving (Show, Eq, Num)

--quantityToLovelace :: Quantity -> Int
--quantityToLovelace (Quantity q) = q

main :: IO ()
main = do
    putStrLn "🚀 Starting server on port 8080"
    scotty 8080 $ do
        -- Define the GET endpoint for health check
        get "/health" $ text "OK"

        -- Define the POST endpoint for summing two numbers
        post "/sum" $ do
            maybeSumRequest <- rescue (Just <$> jsonData) (const $ return Nothing)
            case maybeSumRequest of
                Just (SumRequest x y) -> do
                    json $ object ["result" .= (x + y)]
                Nothing -> do
                    status status400
                    text "Invalid input"

        -- Define the POST endpoint for converting quantity to Lovelace
        post "/lovelace" $ do
            maybeQuantityRequest <- rescue (Just <$> jsonData) (const $ return Nothing)
            case maybeQuantityRequest of
                Just (QuantityRequest qVal) -> do
                    json $ object ["lovelace" .= quantityToLovelace (fromIntegral qVal :: Quantity)]
                Nothing -> do
                    status status400
                    text "Invalid input"

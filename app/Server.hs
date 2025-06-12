{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
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

main :: IO ()
main = do
    putStrLn "🚀 Starting server on port 8080"
    scotty 8080 $ do
        -- Define the GET endpoint for health check
        get "/health" $ text "OK"
        -- Define the POST endpoint for summing two numbers
        post "/sum" $ do
            -- Parse the JSON body into a SumRequest
            maybeSumRequest <- rescue (Just <$> jsonData) (const $ return Nothing)
            case maybeSumRequest of
                Just (SumRequest x y) -> do
                    -- Return the sum as JSON
                    json $ object ["result" .= (x + y)]
                Nothing -> do
                    -- Handle invalid JSON input
                    status status400
                    text "Invalid input"

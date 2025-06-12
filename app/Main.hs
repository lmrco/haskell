{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Web.Scotty

main :: IO ()
main = do
    putStrLn "🚀 Starting server on port 8080"
    scotty 8080 $ do
        get "/health" $ text "OK"

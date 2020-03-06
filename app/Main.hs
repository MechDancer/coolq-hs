{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.IO (hFlush, stdout)


app :: Application
app req respond = do
  getRequestBodyChunk req >>= T.putStrLn.E.decodeUtf8
  hFlush stdout
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"

main :: IO ()
main = do
  putStrLn "http://localhost:8080/"
  run 8080 app
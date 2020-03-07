{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Proxy
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E
import qualified Data.Text.IO             as T
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Types.SourceT    (foreach)
import           System.IO                (hFlush, readFile, stdout)
import           Web.CoolQ.API
import           Web.CoolQ.Data.Common

app :: Application
app req respond = do
  getRequestBodyChunk req >>= T.putStrLn . E.decodeUtf8
  hFlush stdout
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager (BaseUrl Http "192.168.31.85" 5700 "")
  let myQQ = 233
  e <- runClientM (sendPrivateMessage myQQ (textMessage "Hello, world")) env
  print e
  hFlush stdout
  putStrLn "http://localhost:8080/"
  run 8080 app

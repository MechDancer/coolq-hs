{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Servant.Client           (mkClientEnv, runClientM)
import           Servant.Types.SourceT    (foreach)
import           System.IO                (hFlush, readFile, stdout)
import           Web.CoolQ.API
import           Web.CoolQ.Data.Common
import           Web.CoolQ.Data.Post

app :: Application
app req respond = do
  msg :: Either String Post <- eitherDecodeStrict <$> getRequestBodyChunk req
  print msg
  print $ requestHeaders req
  hFlush stdout
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"

main :: IO ()
main
--  manager <- newManager defaultManagerSettings
--  let env = mkClientEnv manager (BaseUrl Http "192.168.31.85" 5700 "")
--  let myQQ = 233
--  e <- runClientM (sendPrivateMessage myQQ (textMessage "Hello, world")) env
--  print e
 = do
  hFlush stdout
  run 8080 app

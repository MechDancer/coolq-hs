{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.CoolQ.API where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client   (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           Servant.Types.SourceT (foreach)
import           Web.CoolQ.Data.API
import           Web.CoolQ.Data.Common

type Param = QueryParam' '[Strict,Required]

type SendAPI
   = "send_private_msg" :> Param "user_id" String :> ReqBody '[JSON] Message :> Get '[JSON] SendPrivateMessageResponse

sendApi :: Proxy SendAPI
sendApi = Proxy

sendPrivateMsg :: String -> Message -> ClientM SendPrivateMessageResponse
sendPrivateMsg = client sendApi

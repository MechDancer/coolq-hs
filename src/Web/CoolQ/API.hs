{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.CoolQ.API where

import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics          (Generic)
import           Servant.API
import           Servant.Client
import           Web.CoolQ.Data.API
import           Web.CoolQ.Data.Common

type Param = QueryParam' '[ Strict, Required]

type GR = Get '[ JSON]

type SendAPI
   = "send_private_msg" :> Param "user_id" Int :> Param "message" Message :> GR SendPrivateMessageResponse :<|> "send_group_msg" :> Param "group_id" Int :> Param "message" Message :> GR SendGroupMessageResponse :<|> "send_discuss_msg" :> Param "discuss_id" Int :> Param "message" Message :> GR SendDiscussMessageResponse :<|> "delete_msg" :> Param "message_id" MessageId :> GR DeleteMessageResponse :<|> "send_like" :> Param "user_id" Int :> Param "times" Int :> GR SendLikeResponse

type SetAPI
   = "set_group_kick" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "reject_add_request" Bool :> GR SetGroupKickResponse :<|> "set_group_ban" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "duration" Int :> GR SetGroupBanResponse :<|> "set_group_anonymous_ban" :> ReqBody '[ JSON] SetGroupAnonymousBanBody :> GR SetGroupAnonymousBanResponse :<|> "set_group_whole_ban" :> Param "group_id" Int :> QueryParam "enable" Bool :> GR SetGroupWholeBanResponse :<|> "set_group_admin" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "enable" Bool :> GR SetGroupWholeBanResponse :<|> "set_group_anonymous" :> Param "group_id" Int :> QueryParam "enable" Bool :> GR SetGroupAnonymousResponse :<|> "set_group_card" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "card" Text :> GR SetGroupCardResponse :<|> "set_group_leave" :> Param "group_id" Int :> QueryParam "is_dismiss" Bool :> GR SetGroupLeaveResponse :<|> "set_group_special_title" :> Param "group_id" Int :> Param "user_id" Int :> Param "special_title" Text :> QueryParam "duration" Int :> GR SetGroupSpecialTitleResponse :<|> "set_discuss_leave" :> Param "discuss_id" Int :> GR SetDiscussLeaveResponse :<|> "set_friend_add_request" :> Param "flag" Text :> QueryParam "approve" Bool :> QueryParam "remark" Text :> GR SetFriendAddRequestResponse :<|> "set_group_add_request" :> Param "flag" Text :> Param "sub_type" Text :> QueryParam "approve" Bool :> QueryParam "reason" Text :> GR SetFriendAddRequestResponse

data SetGroupAnonymousBanBody =
  SetGroupAnonymousBanBody
    { group_id  :: Int
    , flag      :: Text
    , anonymous :: Anonymous
    , duration  :: Maybe Int
    }
  deriving (Show, Generic)

instance ToJSON SetGroupAnonymousBanBody

instance ToHttpApiData Message where
  toQueryParam = encodeCQ

instance ToHttpApiData MessageId where
  toQueryParam (MessageId id) = pack $ show id

sendApi :: Proxy SendAPI
sendApi = Proxy

sendPrivateMessage ::
     Int -- ^ User id
  -> Message -- ^  Message to send
  -> ClientM SendPrivateMessageResponse
sendGroupMessage ::
     Int -- ^ Group id
  -> Message -- ^  Message to send
  -> ClientM SendPrivateMessageResponse
sendDiscussMessage ::
     Int -- ^ Discuss id
  -> Message -- ^  Message to send
  -> ClientM SendDiscussMessageResponse
deleteMessage ::
     MessageId -- ^ Id of message to delete
  -> ClientM DeleteMessageResponse
sendLike ::
     Int -- ^ User id
  -> Int -- ^ Times
  -> ClientM SendLikeResponse
sendPrivateMessage :<|> sendGroupMessage :<|> sendDiscussMessage :<|> deleteMessage :<|> sendLike = client sendApi

setApi :: Proxy SetAPI
setApi = Proxy

setGroupKick ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to kick
  -> Maybe Bool -- ^ Whether reject future requests. Default: `False`
  -> ClientM SetGroupKickResponse
setGroupBan ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to ban
  -> Maybe Int -- ^ Duration (seconds). Default: `30*60`
  -> ClientM SetGroupBanResponse
setGroupAnonymousBan ::
     SetGroupAnonymousBanBody -- ^ Group and anonymous data
  -> ClientM SetGroupAnonymousBanResponse
setGroupWholeBan ::
     Int -- ^ Group id
  -> Maybe Bool -- ^ Enable. Default: `True`
  -> ClientM SetGroupWholeBanResponse
setGroupAdmin ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to set
  -> Maybe Bool -- ^ Enable. Default: `True`
  -> ClientM SetGroupAdminResponse
setGroupAnonymous ::
     Int -- ^ Group id
  -> Maybe Bool -- ^ Enable. Default: `True`
  -> ClientM SetGroupAnonymousResponse
setGroupCard ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to set
  -> Maybe Text -- ^ Card content, empty implies cancel. Default: ``
  -> ClientM SetGroupCardResponse
setGroupLeave ::
     Int -- ^ Group id
  -> Maybe Bool -- ^ Whether dismiss the group (available on owner). Default: `False`
  -> ClientM SetGroupLeaveResponse
setGroupSpecialTitle ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to set
  -> Text -- ^ Title content
  -> Maybe Int -- ^ Duration (seconds). Default: `-1`
  -> ClientM SetGroupSpecialTitleResponse
setDiscussLeave ::
     Int -- ^ Discuss id
  -> ClientM SetDiscussLeaveResponse
setFriendAddRequest ::
     Text -- ^ Flag passed from the request event
  -> Maybe Bool -- ^ Approve. Default: `True`
  -> Maybe Text -- ^ Remark (available on approved)
  -> ClientM SetFriendAddRequestResponse
setGroupAddRequest ::
     Text -- ^ Flag passed from the request event
  -> Text -- ^ Sub type passed from the request event (add or invite)
  -> Maybe Bool -- ^ Approve. Default: `True`
  -> Maybe Text -- ^ Reason (available on disapproved)
  -> ClientM SetGroupAddRequestResponse
setGroupKick :<|> setGroupBan :<|> setGroupAnonymousBan :<|> setGroupWholeBan :<|> setGroupAdmin :<|> setGroupAnonymous :<|> setGroupCard :<|> setGroupLeave :<|> setGroupSpecialTitle :<|> setDiscussLeave :<|> setFriendAddRequest :<|> setGroupAddRequest =
  client setApi

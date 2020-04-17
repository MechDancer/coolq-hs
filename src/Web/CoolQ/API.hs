{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.CoolQ.API where

import           Data.Aeson
import           Data.Char             (toUpper)
import           Data.Proxy
import           Data.Text             hiding (toUpper)
import           GHC.Generics          (Generic)
import           Servant.API
import           Servant.Client
import           Web.CoolQ.Data.API
import           Web.CoolQ.Data.Common

type Param = QueryParam' '[ Strict, Required]

type GR = Get '[ JSON]

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

instance ToHttpApiData RecordFormat where
  toQueryParam format =
    pack $
    case show format of
      (x:xs) -> toUpper x : xs

-----------------------------------------------------------------------------
type SendAPI
   = "send_private_msg" :> Param "user_id" Int :> Param "message" Message :> GR SendPrivateMessageResponse :<|> "send_group_msg" :> Param "group_id" Int :> Param "message" Message :> GR SendGroupMessageResponse :<|> "send_discuss_msg" :> Param "discuss_id" Int :> Param "message" Message :> GR SendDiscussMessageResponse :<|> "delete_msg" :> Param "message_id" MessageId :> GR DeleteMessageResponse :<|> "send_like" :> Param "user_id" Int :> Param "times" Int :> GR SendLikeResponse

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

-----------------------------------------------------------------------------
type SetAPI
   = "set_group_kick" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "reject_add_request" Bool :> GR SetGroupKickResponse :<|> "set_group_ban" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "duration" Int :> GR SetGroupBanResponse :<|> "set_group_anonymous_ban" :> ReqBody '[ JSON] SetGroupAnonymousBanBody :> GR SetGroupAnonymousBanResponse :<|> "set_group_whole_ban" :> Param "group_id" Int :> QueryParam "enable" Bool :> GR SetGroupWholeBanResponse :<|> "set_group_admin" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "enable" Bool :> GR SetGroupWholeBanResponse :<|> "set_group_anonymous" :> Param "group_id" Int :> QueryParam "enable" Bool :> GR SetGroupAnonymousResponse :<|> "set_group_card" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "card" Text :> GR SetGroupCardResponse :<|> "set_group_leave" :> Param "group_id" Int :> QueryParam "is_dismiss" Bool :> GR SetGroupLeaveResponse :<|> "set_group_special_title" :> Param "group_id" Int :> Param "user_id" Int :> Param "special_title" Text :> QueryParam "duration" Int :> GR SetGroupSpecialTitleResponse :<|> "set_discuss_leave" :> Param "discuss_id" Int :> GR SetDiscussLeaveResponse :<|> "set_friend_add_request" :> Param "flag" Text :> QueryParam "approve" Bool :> QueryParam "remark" Text :> GR SetFriendAddRequestResponse :<|> "set_group_add_request" :> Param "flag" Text :> Param "sub_type" Text :> QueryParam "approve" Bool :> QueryParam "reason" Text :> GR SetFriendAddRequestResponse :<|> "set_restart_plugin" :> QueryParam "delay" Int :> GR SetRestartPluginResponse

setApi :: Proxy SetAPI
setApi = Proxy

setGroupKick ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to kick
  -> Maybe Bool -- ^ Whether reject future requests. Default: 'False'
  -> ClientM SetGroupKickResponse
setGroupBan ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to ban
  -> Maybe Int -- ^ Duration (seconds). Default: '30*60'
  -> ClientM SetGroupBanResponse
setGroupAnonymousBan ::
     SetGroupAnonymousBanBody -- ^ Group and anonymous data
  -> ClientM SetGroupAnonymousBanResponse
setGroupWholeBan ::
     Int -- ^ Group id
  -> Maybe Bool -- ^ Enable. Default: 'True'
  -> ClientM SetGroupWholeBanResponse
setGroupAdmin ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to set
  -> Maybe Bool -- ^ Enable. Default: 'True'
  -> ClientM SetGroupAdminResponse
setGroupAnonymous ::
     Int -- ^ Group id
  -> Maybe Bool -- ^ Enable. Default: 'True'
  -> ClientM SetGroupAnonymousResponse
setGroupCard ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to set
  -> Maybe Text -- ^ Card content, empty implies cancel. Default: ''
  -> ClientM SetGroupCardResponse
setGroupLeave ::
     Int -- ^ Group id
  -> Maybe Bool -- ^ Whether dismiss the group (available on owner). Default: 'False'
  -> ClientM SetGroupLeaveResponse
setGroupSpecialTitle ::
     Int -- ^ Group id
  -> Int -- ^ Id of user to set
  -> Text -- ^ Title content
  -> Maybe Int -- ^ Duration (seconds). Default: '-1'
  -> ClientM SetGroupSpecialTitleResponse
setDiscussLeave ::
     Int -- ^ Discuss id
  -> ClientM SetDiscussLeaveResponse
setFriendAddRequest ::
     Text -- ^ Flag passed from the request event
  -> Maybe Bool -- ^ Approve. Default: 'True'
  -> Maybe Text -- ^ Remark (available on approved)
  -> ClientM SetFriendAddRequestResponse
setGroupAddRequest ::
     Text -- ^ Flag passed from the request event
  -> Text -- ^ Sub type passed from the request event (add or invite)
  -> Maybe Bool -- ^ Approve. Default: 'True'
  -> Maybe Text -- ^ Reason (available on disapproved)
  -> ClientM SetGroupAddRequestResponse
setRestartPlugin ::
     Maybe Int -- ^ Delay. Default: 0
  -> ClientM SetRestartPluginResponse
setGroupKick :<|> setGroupBan :<|> setGroupAnonymousBan :<|> setGroupWholeBan :<|> setGroupAdmin :<|> setGroupAnonymous :<|> setGroupCard :<|> setGroupLeave :<|> setGroupSpecialTitle :<|> setDiscussLeave :<|> setFriendAddRequest :<|> setGroupAddRequest :<|> setRestartPlugin =
  client setApi

-----------------------------------------------------------------------------
type GetAPI
   = "get_login_info" :> GR GetLoginInfoResponse :<|> "get_stranger_info" :> Param "user_id" Int :> QueryParam "no_cache" Bool :> GR GetStrangerInfoResponse :<|> "get_friend_list" :> GR GetFriendListResponse :<|> "get_group_list" :> GR GetGroupListResponse :<|> "get_group_info" :> Param "group_id" Int :> QueryParam "no_cache" Bool :> GR GetGroupInfoResponse :<|> "get_group_member_info" :> Param "group_id" Int :> Param "user_id" Int :> QueryParam "no_cache" Bool :> GR GetGroupMemberInfoResponse :<|> "get_group_member_list" :> Param "group_id" Int :> GR GetGroupMemberListResponse :<|> "get_cookies" :> QueryParam "domain" Text :> GR GetCookiesResponse :<|> "get_csrf_token" :> GR GetCSRFTokenResponse :<|> "get_credentials" :> QueryParam "domain" Text :> GR GetCredentialsResponse :<|> "get_record" :> Param "file" FilePath :> Param "out_format" RecordFormat :> QueryParam "full_path" Bool :> GR GetRecordResponse :<|> "get_image" :> Param "file" String :> GR GetImageResponse

getApi :: Proxy GetAPI
getApi = Proxy

getLoginInfo :: ClientM GetLoginInfoResponse
getStrangerInfo :: 
  Int -> -- ^ User id
  Maybe Bool -> -- ^ /No/ caching. Default: 'False'
  ClientM GetStrangerInfoResponse
getFriendList :: ClientM GetFriendListResponse
getGroupList :: ClientM GetGroupListResponse
getGroupInfo :: 
  Int -> -- ^ Group id
  Maybe Bool -> -- ^ /No/ caching. Default: 'False'
  ClientM GetGroupInfoResponse
getGroupMemberInfo :: 
  Int -> -- ^ Group id
  Int -> -- ^ Id of user to get
  Maybe Bool -> -- ^ /No/ caching. Default: 'False'
  ClientM GetGroupMemberInfoResponse
getGroupMemberList :: 
  Int -> -- ^  Group id
  ClientM GetGroupMemberListResponse
getCookies :: 
  Maybe Text -> -- ^ Domain. Default: ''
  ClientM GetCookiesResponse
getCSRFToken :: ClientM GetCSRFTokenResponse
getCredentials :: 
  Maybe Text -> -- ^ Domain. Default: ''
  ClientM GetCredentialsResponse
getRecord :: 
  FilePath -> -- ^ File
  RecordFormat -> -- ^ Output format
  Maybe Bool -> -- Full file path. Default: 'False', recommended to be 'True' on @@Windows@@.
  ClientM GetRecordResponse
getImage :: 
  String -> -- ^ Field @@file@@ in 'ImageMessage'
  ClientM GetImageResponse
getLoginInfo :<|> getStrangerInfo :<|> getFriendList :<|> getGroupList :<|> getGroupInfo :<|> getGroupMemberInfo :<|> getGroupMemberList :<|> getCookies :<|> getCSRFToken :<|> getCredentials :<|> getRecord :<|> getImage =
  client getApi
  

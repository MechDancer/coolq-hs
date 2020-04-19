{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.CoolQ.Data.API where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Web.CoolQ.Data.Common
import           Web.CoolQ.Data.JSONExt

-----------------------------------------------------------------------------
data Response a =
  Response
    { r_status  :: String
    , r_retcode :: Int
    , r_data    :: Maybe a
    }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = parseJSONDrop 2

instance (ToJSON a) => ToJSON (Response a) where
  toJSON = toJSONDrop 2

-----------------------------------------------------------------------------
newtype MessageId =
  MessageId
    { message_id :: Int
    }
  deriving (Show, Generic)

instance FromJSON MessageId

instance ToJSON MessageId

newtype Cookies =
  Cookies
    { cookies :: String
    }
  deriving (Show, Generic)

instance FromJSON Cookies

instance ToJSON Cookies

newtype CSRFToken =
  CSRFToken
    { token :: String
    }
  deriving (Show, Generic)

instance FromJSON CSRFToken

instance ToJSON CSRFToken

newtype File =
  File
    { file :: String
    }
  deriving (Show, Generic)

instance FromJSON File

instance ToJSON File

newtype Boolean =
  Boolean
    { yes :: Bool
    }
  deriving (Show, Generic)

instance FromJSON Boolean

instance ToJSON Boolean

-----------------------------------------------------------------------------
data Credentials =
  Credentials
    { c_cookies :: String
    , c_token   :: String
    }
  deriving (Show, Generic)

data LoginInfo =
  LoginInfo
    { l_user_id  :: Int
    , l_nickName :: Text
    }
  deriving (Show, Generic)

data StrangerInfo =
  StrangerInfo
    { s_user_id  :: Int
    , s_nickName :: Text
    , s_sex      :: Text
    , s_age      :: Int
    }
  deriving (Show, Generic)

data FriendListEntry =
  FriendListEntry
    { fle_user_id  :: Int
    , fle_nickName :: Text
    , fle_remark   :: Text
    }
  deriving (Show, Generic)

data GroupInfo =
  GroupInfo
    { g_group_id         :: Int
    , g_group_name       :: Text
    , g_member_count     :: Int
    , g_max_member_count :: Int
    }
  deriving (Show, Generic)

data GroupListEntry =
  GroupListEntry
    { gle_group_id   :: Int
    , gle_group_name :: Text
    }
  deriving (Show, Generic)

data GroupMemberInfo =
  GroupMemberInfo
    { gm_group_id          :: Int
    , gm_user_id           :: Int
    , gm_nickname          :: Text
    , gm_card              :: Text
    , gm_sex               :: Text
    , gm_age               :: Int
    , gm_area              :: Maybe Text
    , gm_join_time         :: Int
    , gm_last_sent_time    :: Int
    , gm_level             :: Text
    , gm_role              :: Text
    , gm_unfriendly        :: Bool
    , gm_title             :: Maybe Text
    , gm_title_expire_time :: Maybe Int
    , gm_card_changeable   :: Bool
    }
  deriving (Show, Generic)

data VersionInfo =
  VersionInfo
    { coolq_directory            :: String
    , coolq_edition              :: String
    , plugin_version             :: Double
    , plugin_build_number        :: String
    , plugin_build_configuration :: String
    }
  deriving (Show, Generic)

instance FromJSON VersionInfo

instance ToJSON VersionInfo

data RecordFormat
  = Mp3
  | Amr
  | Wma
  | M4a
  | Spx
  | Ogg
  | Wav
  | Flac
  deriving (Eq, Show)

data DataDirType = Image | Record | Show| Bface deriving (Eq,Show)

-----------------------------------------------------------------------------
type EmptyResponse = Response Value

type FriendList = [FriendListEntry]

type GroupMemberList = [GroupMemberInfo]

type GroupList = [GroupListEntry]

-----------------------------------------------------------------------------
type SendPrivateMessageResponse = Response MessageId

type SendGroupMessageResponse = Response MessageId

type SendDiscussMessageResponse = Response MessageId

type DeleteMessageResponse = EmptyResponse

type SendLikeResponse = EmptyResponse

-----------------------------------------------------------------------------
type SetGroupKickResponse = EmptyResponse

type SetGroupBanResponse = EmptyResponse

type SetGroupAnonymousBanResponse = EmptyResponse

type SetGroupWholeBanResponse = EmptyResponse

type SetGroupAdminResponse = EmptyResponse

type SetGroupAnonymousResponse = EmptyResponse

type SetGroupCardResponse = EmptyResponse

type SetGroupLeaveResponse = EmptyResponse

type SetGroupSpecialTitleResponse = EmptyResponse

type SetDiscussLeaveResponse = EmptyResponse

type SetFriendAddRequestResponse = EmptyResponse

type SetGroupAddRequestResponse = EmptyResponse

type SetRestartPluginResponse = EmptyResponse

-----------------------------------------------------------------------------
type GetLoginInfoResponse = Response LoginInfo

type GetStrangerInfoResponse = Response StrangerInfo

type GetFriendListResponse = Response FriendList

type GetGroupListResponse = Response GroupList

type GetGroupInfoResponse = Response GroupInfo

type GetGroupMemberInfoResponse = Response GroupMemberInfo

type GetGroupMemberListResponse = Response GroupMemberList

type GetCookiesResponse = Response Cookies

type GetCSRFTokenResponse = Response CSRFToken

type GetCredentialsResponse = Response Credentials

type GetRecordResponse = Response File

type GetImageResponse = Response File

type GetStatusResponse = Response PluginStatus

type GetVersionInfoResponse = Response VersionInfo

-----------------------------------------------------------------------------
type CanSendImageResponse = Response Boolean

type CanSendRecordResponse = Response Boolean

type CleanDataDirResponse = EmptyResponse

type CleanPluginLogResponse = EmptyResponse

-----------------------------------------------------------------------------
$(generateJSONInstance ''Credentials)

$(generateJSONInstance ''LoginInfo)

$(generateJSONInstance ''StrangerInfo)

$(generateJSONInstance ''FriendListEntry)

$(generateJSONInstance ''GroupInfo)

$(generateJSONInstance ''GroupListEntry)

$(generateJSONInstance ''GroupMemberInfo)

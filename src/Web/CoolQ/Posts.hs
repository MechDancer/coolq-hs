{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Web.CoolQ.Posts where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Web.CoolQ.CommonData
import           Web.CoolQ.JSONExt

-----------------------------------------------------------------------------

type family ResponseP post where
  ResponseP PrivateMessagePost = PrivateMessageReply
  ResponseP GroupMessagePost = GroupMessageReply
  ResponseP DiscussMessagePost = DiscussMessageReply
  ResponseP FriendAddRequestPost = FriendAddRequestReply
  ResponseP GroupAddRequestPost = GroupAddRequestReply

-----------------------------------------------------------------------------

data Post =
  Post
    { private_message       :: Maybe PrivateMessagePost
    , group_message         :: Maybe GroupMessagePost
    , discuss_message       :: Maybe DiscussMessagePost
    , group_file_upload     :: Maybe GroupFileUploadPost
    , group_admin_change    :: Maybe GroupAdminChangePost
    , group_member_decrease :: Maybe GroupMemberDecreasePost
    , group_member_increase :: Maybe GroupMemberIncreasePost
    , group_ban             :: Maybe GroupBanPost
    , group_add_request     :: Maybe GroupAddRequestPost
    , friend_add            :: Maybe FriendAddPost
    , friend_add_request    :: Maybe FriendAddRequestPost
    }
  deriving (Show, Generic)

emptyPost =
  Post
    { private_message = Nothing
    , group_message = Nothing
    , discuss_message = Nothing
    , group_file_upload = Nothing
    , group_admin_change = Nothing
    , group_member_decrease = Nothing
    , group_member_increase = Nothing
    , group_ban = Nothing
    , group_add_request = Nothing
    , friend_add = Nothing
    , friend_add_request = Nothing
    }

instance FromJSON Post where
  parseJSON v@(Object obj) = do
    post_type <- obj .: "post_type" :: Parser Value
    case post_type of
      String "message" -> do
        message_type <- obj .: "message_type" :: Parser Value
        case message_type of
          String "private" -> parseJSON v >>= \m -> return emptyPost {private_message = m}
          String "group" -> parseJSON v >>= \m -> return emptyPost {group_message = m}
          String "discuss" -> parseJSON v >>= \m -> return emptyPost {discuss_message = m}
          _ -> prependFailure "Unknown type" (unexpected message_type)
      String "notice" -> do
        notice_type <- obj .: "notice_type" :: Parser Value
        case notice_type of
          String "group_upload" -> parseJSON v >>= \m -> return emptyPost {group_file_upload = m}
          String "group_admin" -> parseJSON v >>= \m -> return emptyPost {group_admin_change = m}
          String "group_decrease" -> parseJSON v >>= \m -> return emptyPost {group_member_decrease = m}
          String "group_increase" -> parseJSON v >>= \m -> return emptyPost {group_member_increase = m}
          String "group_ban" -> parseJSON v >>= \m -> return emptyPost {group_ban = m}
          String "friend_add" -> parseJSON v >>= \m -> return emptyPost {friend_add = m}
          _ -> prependFailure "Unknown type" (unexpected notice_type)
      String "request" -> do
        request_type <- obj .: "request_type" :: Parser Value
        case request_type of
          String "friend" -> parseJSON v >>= \m -> return emptyPost {friend_add_request = m}
          String "group" -> parseJSON v >>= \m -> return emptyPost {group_add_request = m}
      _ -> prependFailure "Unknown type" (unexpected post_type)
  parseJSON invalid = prependFailure "parsing Post failed, " (typeMismatch "Object" invalid)

-----------------------------------------------------------------------------

data PrivateMessagePost =
  PrivateMessagePost
    { p_post_type    :: Text
    , p_message_type :: Text
    , p_sub_type     :: Text
    , p_message_id   :: Int
    , p_user_id      :: Int
    , p_message      :: Message
    , p_raw_message  :: Text
    , p_font         :: Int
    , p_sender       :: PrivateMessageSender
    }
  deriving (Show, Generic)

data PrivateMessageSender =
  PrivateMessageSender
    { ps_user_id  :: Maybe Int
    , ps_nickname :: Maybe Text
    , ps_sex      :: Maybe Text
    , ps_age      :: Maybe Int
    }
  deriving (Show, Generic)

newtype PrivateMessageReply =
  PrivateMessageReply
    { p_reply :: Message
    }
  deriving (Show, Generic)

-----------------------------------------------------------------------------

data GroupMessagePost =
  GroupMessagePost
    { g_post_type    :: Text
    , g_message_type :: Text
    , g_sub_type     :: Text
    , g_message_id   :: Int
    , g_group_id     :: Int
    , g_user_id      :: Int
    , g_message      :: Message
    , g_anonymous    :: Anonymous
    , g_raw_message  :: Text
    , g_font         :: Int
    , g_sender       :: GroupMessageSender
    }
  deriving (Show, Generic)

data GroupMessageSender =
  GroupMessageSender
    { gs_user_id  :: Maybe Int
    , gs_nickname :: Maybe Text
    , gs_card     :: Maybe Text
    , gs_sex      :: Maybe Text
    , gs_age      :: Maybe Int
    , gs_area     :: Maybe Text
    , gs_level    :: Maybe Text
    , gs_role     :: Maybe Text
    , gs_title    :: Maybe Text
    }
  deriving (Show, Generic)

data GroupMessageReply =
  GroupMessageReply
    { g_reply        :: Message
    , g_at_sender    :: Bool
    , g_delete       :: Bool
    , g_kick         :: Bool
    , g_ban          :: Bool
    , g_ban_duration :: Int
    }
  deriving (Show, Generic)

-----------------------------------------------------------------------------

data DiscussMessagePost =
  DiscussMessagePost
    { d_post_type    :: Text
    , d_message_type :: Text
    , d_message_id   :: Int
    , d_discuss_id   :: Int
    , d_user_id      :: Int
    , d_message      :: Message
    , d_raw_message  :: Text
    , d_font         :: Int
    , d_sender       :: DiscussMessageSender
    }
  deriving (Show, Generic)

data DiscussMessageSender =
  DiscussMessageSender
    { ds_user_id  :: Maybe Int
    , ds_nickname :: Maybe Text
    , ds_sex      :: Maybe Text
    , ds_age      :: Maybe Int
    }
  deriving (Show, Generic)

data DiscussMessageReply =
  DiscussMessageReply
    { d_reply     :: Message
    , d_at_sender :: Bool
    }
  deriving (Show, Generic)

-----------------------------------------------------------------------------

data GroupFileUploadPost =
  GroupFileUploadPost
    { gfu_post_type   :: Text
    , gfu_notice_type :: Text
    , gfu_group_id    :: Int
    , gfu_user_id     :: Int
    , gfu_file        :: GroupFile
    }
  deriving (Show, Generic)

data GroupFile =
  GroupFile
    { gf_id    :: Int
    , gf_name  :: Text
    , gf_size  :: Int
    , gf_busid :: Int
    }
  deriving (Show, Generic)

data GroupAdminChangePost =
  GroupAdminChangePost
    { gac_post_name   :: Text
    , gac_notice_type :: Text
    , gac_sub_type    :: Text
    , gac_group_id    :: Int
    , gac_user_id     :: Int
    }
  deriving (Show, Generic)

data GroupMemberDecreasePost =
  GroupMemberDecreasePost
    { gmd_post_name   :: Text
    , gmd_notice_type :: Text
    , gmd_sub_type    :: Text
    , gmd_group_id    :: Int
    , gmd_operator_id :: Int
    , gmd_user_id     :: Int
    }
  deriving (Show, Generic)

data GroupMemberIncreasePost =
  GroupMemberIncreasePost
    { gid_post_name   :: Text
    , gid_notice_type :: Text
    , gid_sub_type    :: Text
    , gid_group_id    :: Int
    , gid_operator_id :: Int
    , gid_user_id     :: Int
    }
  deriving (Show, Generic)

data GroupBanPost =
  GroupBanPost
    { gb_post_name   :: Text
    , gb_notice_type :: Text
    , gb_sub_type    :: Text
    , gb_group_id    :: Int
    , gb_operator_id :: Int
    , gb_user_id     :: Int
    , gb_duration    :: Int
    }
  deriving (Show, Generic)

-----------------------------------------------------------------------------

data FriendAddPost =
  FriendAddPost
    { fa_post_name   :: Text
    , fa_notice_type :: Text
    , fa_user_id     :: Int
    }
  deriving (Show, Generic)

data FriendAddRequestPost =
  FriendAddRequestPost
    { far_post_type    :: Text
    , far_request_type :: Text
    , far_user_id      :: Int
    , far_comment      :: Text
    , far_flag         :: Text
    }
  deriving (Show, Generic)

data FriendAddRequestReply =
  FriendAddRequestReply
    { far_approve :: Bool
    , far_remark  :: Text
    }
  deriving (Show, Generic)

-----------------------------------------------------------------------------

data GroupAddRequestPost =
  GroupAddRequestPost
    { gar_post_type    :: Text
    , gar_request_type :: Text
    , gar_sub_type     :: Text
    , gar_group_id     :: Int
    , gar_user_id      :: Int
    , gar_comment      :: Text
    , gar_flag         :: Text
    }
  deriving (Show, Generic)

data GroupAddRequestReply =
  GroupAddRequestReply
    { gar_approve :: Bool
    , gar_remark  :: Text
    }
  deriving (Show, Generic)

-----------------------------------------------------------------------------

data Message
  = TextMessage
      { t_type :: Text
      , t_data :: TextMessageData
      }
  | ImageMessage
      { i_type :: Text
      , i_data :: ImageMessageData
      }
  | FaceMessage
      { f_type :: Text
      , f_data :: FaceMessageData
      }
  | Message [Message]
  deriving (Show, Generic)

newtype TextMessageData =
  TextMessageData
    { t_text :: Text
    }
  deriving (Show, Generic)

data ImageMessageData =
  ImageMessageData
    { i_file :: Text
    , i_url  :: Text
    }
  deriving (Show, Generic)

newtype FaceMessageData =
  FaceMessageData
    { f_id :: Text
    }
  deriving (Show, Generic)

-----------------------------------------------------------------------------

data LifecyclePost =
  LifecyclePost
    { l_post_type       :: Text
    , l_meta_event_type :: Text
    , l_sub_type        :: Text
    }
  deriving (Show, Generic)

data HeartbeatPost =
  HeartbeatPost
    { h_post_type       :: Text
    , h_meta_event_type :: Text
    , h_status          :: PluginStatus
    , h_interval        :: Int
    }
  deriving (Show, Generic)
  
-----------------------------------------------------------------------------

$(generateJSONInstance ''FaceMessageData)

$(generateJSONInstance ''ImageMessageData)

$(generateJSONInstance ''TextMessageData)

$(generateJSONInstance ''Message)

$(generateJSONInstance ''GroupAddRequestReply)

$(generateJSONInstance ''GroupAddRequestPost)

$(generateJSONInstance ''FriendAddPost)

$(generateJSONInstance ''FriendAddRequestPost)

$(generateJSONInstance ''FriendAddRequestReply)

$(generateJSONInstance ''GroupAdminChangePost)

$(generateJSONInstance ''GroupMemberIncreasePost)

$(generateJSONInstance ''GroupMemberDecreasePost)

$(generateJSONInstance ''GroupFile)

$(generateJSONInstance ''GroupFileUploadPost)

$(generateJSONInstance ''GroupBanPost)

$(generateJSONInstance ''PrivateMessageSender)

$(generateJSONInstance ''GroupMessageSender)

$(generateJSONInstance ''DiscussMessageSender)

$(generateJSONInstance ''DiscussMessagePost)

$(generateJSONInstance ''GroupMessagePost)

$(generateJSONInstance ''PrivateMessagePost)

$(generateJSONInstance ''PrivateMessageReply)

$(generateJSONInstance ''DiscussMessageReply)

$(generateJSONInstance ''LifecyclePost)

$(generateJSONInstance ''HeartbeatPost)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.CoolQ.Posts where

import Web.CoolQ.JSONExt
import Data.Aeson.Types(FromJSON,ToJSON)
import Data.Aeson
import GHC.Generics (Generic)


data PrivateMessagePost =
  PrivateMessagePost
    { p_post_type :: String
    , p_message_type :: String
    , p_sub_type :: String
    , p_message_id :: Int
    , p_user_id :: Int
    , p_message :: Message
    , p_raw_message :: String
    , p_font :: Int
    , p_sender :: PrivateMessageSender
    }
  deriving (Show, Generic)

data PrivateMessageSender =
  PrivateMessageSender
    { ps_user_id :: Maybe Int
    , ps_nickname :: Maybe String
    , ps_sex :: Maybe String
    , ps_age :: Maybe Int
    }
  deriving (Show, Generic)

newtype PrivateMessageReply =
  PrivateMessageReply
    { p_reply :: Message
    }deriving (Show, Generic)

data GroupMessagePost =
  GroupMessagePost
    { g_post_type :: String
    , g_message_type :: String
    , g_sub_type :: String
    , g_message_id :: Int
    , g_group_id :: Int
    , g_user_id :: Int
    , g_message :: Message
    , g_anonymous :: Anonymous
    , g_raw_message :: String
    , g_font :: Int
    , g_sender :: GroupMessageSender
    }deriving (Show, Generic)

data Anonymous =
  Anonymous
    { a_id :: Int
    , a_name :: String
    , a_flag :: String
    }deriving (Show, Generic)

data GroupMessageSender =
  GroupMessageSender
    { gs_user_id :: Maybe Int
    , gs_nickname :: Maybe String
    , gs_card :: Maybe String
    , gs_sex :: Maybe String
    , gs_age :: Maybe Int
    , gs_area :: Maybe String
    , gs_level :: Maybe String
    , gs_role :: Maybe String
    , gs_title :: Maybe String
    }
  deriving (Show, Generic)

data GroupMessageReply =
  GroupMessageReply
    { g_reply :: Message
    , g_at_sender :: Bool
    , g_delete :: Bool
    , g_kick :: Bool
    , g_ban :: Bool
    , g_ban_duration :: Int
    }deriving (Show, Generic)

data DiscussMessagePost =
  DiscussMessagePost
    { d_post_type :: String
    , d_message_type :: String
    , d_message_id :: Int
    , d_discuss_id :: Int
    , d_user_id :: Int
    , d_message :: Message
    , d_raw_message :: String
    , d_font :: Int
    , d_sender :: DiscussMessageSender
    }
  deriving (Show, Generic)

data DiscussMessageSender =
  DiscussMessageSender
    { ds_user_id :: Maybe Int
    , ds_nickname :: Maybe String
    , ds_sex :: Maybe String
    , ds_age :: Maybe Int
    }
  deriving (Show, Generic)

data DiscussMessageReply =
  DiscussMessageReply
    { d_reply :: Message
    , d_at_sender :: Bool
    }
  deriving (Show, Generic)

data GroupFileUploadPost =
  GroupFileUploadPost
    { gfu_post_type :: String
    , gfu_notice_type :: String
    , gfu_group_id :: Int
    , gfu_user_id :: Int
    , gfu_file :: GroupFile
    }
  deriving (Show, Generic)

data GroupFile =
  GroupFile
    { gf_id :: Int
    , gf_name :: String
    , gf_size :: Int
    , gf_busid :: Int
    }
  deriving (Show, Generic)

data GroupAdminChangePost =
  GroupAdminChangePost
    { gac_post_name :: String
    , gac_notice_type :: String
    , gac_sub_type :: String
    , gac_group_id :: Int
    , gac_user_id :: Int
    }
  deriving (Show, Generic)

data GroupMemberDecreasePost =
  GroupMemberDecreasePost
    { gmd_post_name :: String
    , gmd_notice_type :: String
    , gmd_sub_type :: String
    , gmd_group_id :: Int
    , gmd_operator_id :: Int
    , gmd_user_id :: Int
    }
  deriving (Show, Generic)

data GroupMemberIncreasePost =
  GroupMemberIncreasePost
    { gid_post_name :: String
    , gid_notice_type :: String
    , gid_sub_type :: String
    , gid_group_id :: Int
    , gid_operator_id :: Int
    , gid_user_id :: Int
    }
  deriving (Show, Generic)

data GroupBanPost =
  GroupBanPost
    { gb_post_name :: String
    , gb_notice_type :: String
    , gb_sub_type :: String
    , gb_group_id :: Int
    , gb_operator_id :: Int
    , gb_user_id :: Int
    , gb_duration :: Int
    }
  deriving (Show, Generic)

data FriendAddPost =
  FriendAddPost
    { fa_post_name :: String
    , fa_notice_type :: String
    , fa_user_id :: Int
    }
  deriving (Show, Generic)

data FriendAddRequestPost =
  FriendAddRequestPost
    { far_post_type :: String
    , far_request_type :: String
    , far_user_id :: Int
    , far_comment :: String
    , far_flag :: String
    }
  deriving (Show, Generic)

data FriendAddRequestReply =
  FriendAddRequestReply
    { far_approve :: Bool
    , far_remark :: String
    }
  deriving (Show, Generic)

data GroupAddRequestPost =
  GroupAddRequestPost
    { gar_post_type :: String
    , gar_request_type :: String
    , gar_sub_type :: String
    , gar_group_id :: Int
    , gar_user_id :: Int
    , gar_comment :: String
    , gar_flag :: String
    }
  deriving (Show, Generic)

data GroupAddRequestReply =
  GroupAddRequestReply
    { gar_approve :: Bool
    , gar_remark :: String
    }
  deriving (Show, Generic)

data Message
  = TextMessage
      { t_type :: String
      , t_data :: TextMessageData
      }
  | ImageMessage
      { i_type :: String
      , i_data :: ImageMessageData
      }
  | FaceMessage
      { f_type :: String
      , f_data :: FaceMessageData
      }
  | Message [Message]
  deriving (Show, Generic)


newtype TextMessageData =
  TextMessageData
    { t_text :: String
    }
  deriving (Show, Generic)
  

data ImageMessageData =
  ImageMessageData
    { i_file :: String
    , i_url :: String
    }
  deriving (Show, Generic)


newtype FaceMessageData =
  FaceMessageData
    { f_id :: String
    }
  deriving (Show, Generic)

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
$(generateJSONInstance ''PrivateMessageSender)
$(generateJSONInstance ''GroupMessageSender)
$(generateJSONInstance ''DiscussMessageSender)
$(generateJSONInstance ''Anonymous)
$(generateJSONInstance ''DiscussMessagePost)
$(generateJSONInstance ''GroupMessagePost)
$(generateJSONInstance ''PrivateMessagePost)
$(generateJSONInstance ''PrivateMessageReply)
$(generateJSONInstance ''DiscussMessageReply)

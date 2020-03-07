{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.CoolQ.Data.Common where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text         (Text)
import           GHC.Generics      (Generic)
import           Web.CoolQ.Data.JSONExt

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

data Anonymous =
  Anonymous
    { a_id   :: Int
    , a_name :: Text
    , a_flag :: Text
    }
  deriving (Show, Generic)

data PluginStatus =
  PluginStatus
    { app_initialized :: Bool
    , app_enabled     :: Bool
    , plugins_good    :: Object
    , app_good        :: Bool
    , online          :: Bool
    , good            :: Bool
    }
  deriving (Show, Generic)

instance FromJSON PluginStatus

instance ToJSON PluginStatus

$(generateJSONInstance ''Anonymous)

$(generateJSONInstance ''FaceMessageData)

$(generateJSONInstance ''ImageMessageData)

$(generateJSONInstance ''TextMessageData)

$(generateJSONInstance ''Message)

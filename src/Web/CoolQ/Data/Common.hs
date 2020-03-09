{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.CoolQ.Data.Common where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text              (Text, append)
import           GHC.Generics           (Generic)
import           Web.CoolQ.Data.JSONExt

textMessage ::
     Text -- ^ Content text
  -> Message
textMessage t = TextMessage "text" $ TextMessageData t

imageMessage ::
     Text -- ^ Image path
  -> Message
imageMessage file = ImageMessage "image" $ ImageMessageData file Nothing

faceMessage ::
     Text -- ^ Face id
  -> Message
faceMessage id = FaceMessage "face" $ FaceMessageData id


instance Semigroup Message where
  Message m <> Message n = Message $ m <> n
  Message m <> n = Message $ m <> [n]
  m <> Message n = Message $ m : n
  m <> n = Message [m, n]

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

encodeCQ :: Message -> Text
encodeCQ (TextMessage _ (TextMessageData t)) = t
encodeCQ (ImageMessage _ (ImageMessageData file url)) =
  let content =
        case url of
          Just url' -> url' `append` "/" `append` file
          Nothing   -> file
   in append "[CQ:image,file=" content `append` "]"
encodeCQ (FaceMessage _ (FaceMessageData id)) = "[CQ:face,id=" `append` id `append` "]"
encodeCQ (Message []) = ""
encodeCQ (Message (x:xs)) = encodeCQ x `append` encodeCQ (Message xs)

newtype TextMessageData =
  TextMessageData
    { t_text :: Text
    }
  deriving (Show, Generic)

data ImageMessageData =
  ImageMessageData
    { i_file :: Text
    , i_url  :: Maybe Text
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

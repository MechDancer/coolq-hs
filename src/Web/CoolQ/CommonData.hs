{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.CoolQ.CommonData where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text         (Text)
import           GHC.Generics      (Generic)
import           Web.CoolQ.JSONExt

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

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes.OptionalFields (demo) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           GHC.Generics

data Workspace = Workspace
  { folders  :: [String]
  , settings :: Value
  } deriving (Show, Generic)

instance FromJSON Workspace where
  parseJSON = withObject "Workspace" $ \o -> do
    folders <- o .: "folders"
    settings <- o .:? "settings" .!= (Object HM.empty)
    return $ Workspace folders settings
instance ToJSON Workspace

demo :: IO ()
demo = do
  print "/// optional fields"
  let s = decode "{\"folders\": [\"iddqd\"]}" :: Maybe Workspace
  print s
  print . encode $ s

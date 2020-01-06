{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ConfigDemo (demo) where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import           Data.Yaml           (FromJSON (..), (.:))
import qualified Data.Yaml           as Y
import           Prelude
import           Text.RawString.QQ

configYaml :: ByteString
configYaml = [r|
resolver: lts-3.7
packages:
    - ./yesod-core
    - ./yesod-static
    - ./yesod-persistent
    - ./yesod-newsfeed
    - ./yesod-form
    - ./yesod-auth
    - ./yesod-auth-oauth
    - ./yesod-sitemap
    - ./yesod-test
    - ./yesod-bin
    - ./yesod
    - ./yesod-eventsource
    - ./yesod-websockets

# Needed for LTS 2
extra-deps:
- wai-app-static-3.1.4.1
|]

data Config =
    Config {
    resolver    :: Text
    , packages  :: [FilePath]
    , extraDeps :: [Text]
    } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .:   "resolver"       <*>
    v .:   "packages" <*>
    v .:   "extra-deps"
  parseJSON _ = fail "Expected Object for Config value"

demo :: IO ()
demo = do
    config <- Y.decodeThrow configYaml
    print (config :: Config)

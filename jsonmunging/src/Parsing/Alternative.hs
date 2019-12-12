{-# LANGUAGE OverloadedStrings #-}

module Parsing.Alternative (demo) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Aeson
import           Data.Foldable       (asum)
import           Text.Read           (readMaybe)

data MapData = MapData
  { name :: String
  , code :: String
  } deriving (Show)

instance FromJSON MapData where
  parseJSON = withObject "MapData" $ \o -> do
    mapName <- o .: "name"

    -- basic applicative style
    -- mapCode <- o .: "code" <|>  o .: "MAP_CODE"

    -- basic asum style
    -- mapCode <- asum [o .: "code", o .: "map_code", o .: "MAP_CODE"]

    -- advanced asum style
    mapCode <- asum
      [ o .: "code"  -- lower case
      , o .: "MAP_CODE"  -- upper case
      , do v <- o .: "map_num"  -- number
           case (readMaybe v) :: Maybe Int of
              Nothing -> fail "not a number"
              Just x  -> return $ show x
      , do guard (mapName == "debuglevel")
           return "dev_level"
      ]

    return $ MapData mapName mapCode

demo :: IO ()
demo = do
  print "/// alternative parsing"
  print (decode "{\"name\": \"hell's gate\", \"code\": \"e1m1\"}" :: Maybe MapData)
  print (decode "{\"name\": \"hell's gate\", \"MAP_CODE\": \"e1m1\"}" :: Maybe MapData)
  print (decode "{\"name\": \"debuglevel\"}" :: Maybe MapData)
  print (decode "{\"name\": \"hell's gate\", \"map_num\": \"131\"}" :: Maybe MapData)


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes.DeriveGenericTH (demo) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromJust)

data MapData = MapData
  { name :: Maybe String
  , code :: Maybe String
  } deriving (Show)

deriveJSON defaultOptions ''MapData

demo :: IO ()
demo = do
  print "/// use TemplateHaskell ext to derive generic"
  let s = decode "{}" :: Maybe MapData
  print . fromJust $ s
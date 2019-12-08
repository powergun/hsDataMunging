{-# LANGUAGE OverloadedStrings #-}

module StandardTypes.List (demo) where

import           Data.Aeson
import           Data.Map
import           Data.Text

intsRoundTrip :: IO ()
intsRoundTrip = do
  print (decode "[1, 2, 3]" :: Maybe [Int])

demo :: IO ()
demo = do
  intsRoundTrip

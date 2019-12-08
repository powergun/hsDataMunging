{-# LANGUAGE OverloadedStrings #-}
module StandardTypes.Scalar (demo) where

import           Data.Aeson
import           Data.Text

booleanRoundTrip :: IO ()
booleanRoundTrip = do
  print (decode "true" :: Maybe Bool)
  print $ encode True

demo :: IO ()
demo = do
  booleanRoundTrip

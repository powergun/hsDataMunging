{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Herestring.InterpolatedNeat (demo) where

import           Data.Text         (Text)
import           NeatInterpolation

configTemplate :: Text -> Text -> Text
configTemplate acc secr = [text|
[profile]
aws_access_key_id = ${acc}
aws_secret_access_key = ${secr}
|]

demo :: IO ()
demo = do
  print $ configTemplate "12312" "Kh3241"

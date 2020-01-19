{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Herestring.InterpolatedHere (demo) where

import qualified Data.String.Here.Interpolated as HereIn
import qualified Data.Text                     as T

configTemplateT :: T.Text -> T.Text -> T.Text
configTemplateT acc secr = [HereIn.i|
[profile]
aws_access_key_id = ${acc}
aws_secret_access_key = ${secr}
|]

configTemplateStr :: String -> String -> String
configTemplateStr acc secr = [HereIn.i|
[profile]
aws_access_key_id = ${acc}
aws_secret_access_key = ${secr}
|]

demo :: IO ()
demo = do
  print $ configTemplateT "12312" "Kh3241"
  print $ configTemplateStr "12312" "Kh3241"

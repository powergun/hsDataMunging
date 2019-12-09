{-# LANGUAGE OverloadedStrings #-}
module AnyTypes.Value (demo) where

import           Data.Aeson

decodeToValue :: FilePath -> IO ()
decodeToValue filename = do
  let s' = "{\"folders\":[{\"path\":\"javascript/github.com/Wei-N-Ning/jsExamples/serverless/serverless-big-mouths\"}],\"settings\":{\"editor.tabSize\":2}}"
  print (decode s' :: Maybe Value)

demo :: IO ()
demo = do
  decodeToValue "testdata/anytypes.json"

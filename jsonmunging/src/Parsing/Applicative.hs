{-# LANGUAGE OverloadedStrings #-}
module Parsing.Applicative (demo) where

import           Data.Aeson
import           Data.Aeson.Types

parseTuple :: Value -> Parser (String, Bool)
parseTuple = withObject "tuple" $ \o ->
  (,) <$> o .: "a"
      <*> o .: "b"

demo :: IO ()
demo = do
  print $ parseMaybe parseTuple
        =<< (decode "{\"a\": \"e1m1\", \"b\": false}" :: Maybe Value)

{-# LANGUAGE OverloadedStrings #-}

module Parsing.WithParsing (demo) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

parseTuple :: Value -> Parser (String, Bool)
parseTuple = withObject "tuple" $ \obj -> do
  -- Parse "a".
  a <- case HM.lookup "a" obj of
    Just x  -> parseJSON x
    Nothing -> fail "no field 'a'"

  -- Parse "b".
  b <- case HM.lookup "b" obj of
    Just x  -> parseJSON x
    Nothing -> fail "no field 'b'"

  -- That's all!
  return (a, b)

demo :: IO ()
demo = do
  let parsing :: T.Text -> Parser String
      parsing t = let ss = T.unpack $ t
                  in case ss == "e1m1" of True  -> return ss
                                          False -> return "..."
  print "/// with-parsing"
  print $ parseMaybe (withText "not iddqd" parsing)
        =<< (decode "\"e1m1\"" :: Maybe Value)
  print $ parseMaybe (withText "not iddqd" parsing)
        =<< (decode "\"hell's gate\"" :: Maybe Value)

{-# LANGUAGE OverloadedStrings #-}

module Parsing.FromScratch (demo) where

-- source: https://artyom.me/aeson

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Vector         as V

parseTuple :: Value -> Parser (String, Bool)
parseTuple (Object obj) = do
  -- Look up the "a" field.
  let mbFieldA = HM.lookup "a" obj

  -- Fail if it wasn't found.
  fieldA <- case mbFieldA of
    Just x  -> return x
    Nothing -> fail "no field 'a'"

  -- Extract the value from it, or fail if it's of the wrong type.
  a <- case fieldA of
    String x -> return (T.unpack x)
    _        -> fail "expected a string"

  -- Do all the same for "b" (in a slightly terser way, to save space):
  b <- case HM.lookup "b" obj of
    Just (Bool x) -> return x
    Just _        -> fail "expected a boolean"
    Nothing       -> fail "no field 'b'"

  -- That's all!
  return (a, b)
parseTuple _ = fail "iddqd"

parseArray :: Value -> Parser [(String, Bool)]
parseArray (Array arr) = mapM parseTuple (V.toList arr)
parseArray _           = fail "idkfa"

demo :: IO ()
demo = do
  let s = decode "{\"a\": \"iddqd\", \"b\": true}" :: Maybe Value
  print "/// FromScratch"
  print (parseMaybe parseTuple =<< s)

  let r = decode "[{\"a\": \"iddqd\", \"b\": true}, {\"a\": \"iddqd\", \"b\": true}]" :: Maybe Value
  print (parseMaybe parseArray =<< r)

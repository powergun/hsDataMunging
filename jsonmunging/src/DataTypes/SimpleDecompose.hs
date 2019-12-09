{-# LANGUAGE OverloadedStrings #-}

module DataTypes.SimpleDecompose (demo) where

import           Data.Aeson
import qualified Data.Aeson.Types as AT

data Person = Person
  { name :: String
  , age  :: Object
  } deriving (Show)

decompose :: Maybe Person
decompose = do
  result <- decode "{\"name\":\"Dave\",\"age\":[1, 2, 3]}"
  s <- flip AT.parseMaybe result $ \obj -> do
                          name <- obj .: "name"
                          age <- obj .: "age"
                          return $ Person name (age :: Object)
  return s

demo :: IO ()
demo =
  print decompose


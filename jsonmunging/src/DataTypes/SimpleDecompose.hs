{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes.SimpleDecompose (demo) where

import           Data.Aeson
import qualified Data.Aeson.Types as AT
import           GHC.Generics

data Person = Person
  { name :: String
  , age  :: Value
  } deriving (Show)

data Person1 = Person1
  { name1 :: String
  , age1  :: Value
  } deriving (Generic, Show)

instance FromJSON Person1
instance ToJSON Person1

decompose :: Maybe Person
decompose = do
  result <- decode "{\"name\":\"Dave\",\"age\":[1, 2, 3]}"
  s <- flip AT.parseMaybe result $ \obj -> do
                          name <- obj .: "name"
                          age <- obj .: "age"
                          return $ Person name (age :: Value)
  return s

decomposeWithType :: IO ()
decomposeWithType = do
  let s = "{\"name1\":\"Dave\",\"age1\":[1, 2, 3]}"
      p = decode s :: Maybe Person1
  print p


demo :: IO ()
demo = do
  print decompose
  decomposeWithType

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes.Person (demo) where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person

demo :: IO ()
demo = do
  putStrLn $ "Encode: " ++
    (show (encode (Person { name = "Joe", age = 12 })))
  putStrLn $ "Decode: " ++
    (show (decode "{ \"name\": \"Joe\", \"age\": 12 }" :: Maybe Person))

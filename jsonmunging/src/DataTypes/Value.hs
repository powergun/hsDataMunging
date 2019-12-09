{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes.Value (demo) where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Person = Person
  { name :: Text
  , age  :: Object
  } deriving (Show)

instance FromJSON Person where
  parseJSON = withObject "Person" $ \o -> do
      name_ <- o .: "name"
      return $ Person name_ o

demo :: IO ()
demo = do
  -- putStrLn $ "Encode: " ++
  --   (show (encode (Person { name = "Joe", age = 12 })))
  putStrLn $ "Decode: " ++
    (show (decode "{ \"name\": \"Joe\", \"age\": 12 }" :: Maybe Person))

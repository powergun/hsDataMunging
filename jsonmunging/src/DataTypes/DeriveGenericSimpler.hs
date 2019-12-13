{-# LANGUAGE DeriveGeneric, DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes.DeriveGenericSimpler
  ( demo
  )
where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, ToJSON, FromJSON, Show)

demo :: IO ()
demo = do
  putStrLn $ "Encode: " ++ (show (encode (Person { name = "Joe", age = 12 })))
  putStrLn
    $  "Decode: "
    ++ (show (decode "{ \"name\": \"Joe\", \"age\": 12 }" :: Maybe Person))

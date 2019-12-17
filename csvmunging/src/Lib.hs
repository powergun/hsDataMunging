module Lib
    ( someFunc
    ) where

import           Data.Text (Text)
import           Data.Time (Day)

data LedgerLine = LedgerLine {
    date    :: Day
, amount    :: Amount
, reference :: Reference
, category  :: Maybe Category
} deriving (Eq, Show)

data Category = Category Text
    deriving (Eq, Show)

data Reference = Reference Text
    deriving (Eq, Show)

data Amount = Amount Double
    deriving (Eq, Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

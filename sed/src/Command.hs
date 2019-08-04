module Command
  ( Command(..)
  ) where

data Command = Print
             | Next
             | Delete
             | Substitute String String String
             deriving (Show)

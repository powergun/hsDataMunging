#!/usr/bin/env stack runghc

import Data.List

formatList :: String -> String -> String -> [String] -> String
formatList s e sep xs =
  s ++ (intercalate sep (map show xs)) ++ e

main :: IO ()
main = do
  putStrLn $ formatList 
    "<l>" "</l>" ", " ["there", "is", "a cow"]
  -- real world haskell P113
  -- this is not string concatenation but it is a legit syntax
  -- to describe the composition of a string, which is useful
  -- in pattern match (split a string and match the <prefix, 
  -- suffix> pair)
  print ('\r':'\n':"asd")

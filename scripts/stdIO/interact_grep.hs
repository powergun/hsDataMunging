#!/usr/bin/env stack runghc

_filter :: String -> Bool
_filter line =
  let elems = words line
  in case (elems !! 1 == "console") of
       True -> True
       otherwise -> False

_main :: String -> String
_main = unlines . (filter _filter) . lines

main :: IO ()
main = do
  interact _main

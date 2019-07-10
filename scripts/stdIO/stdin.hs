#!/usr/bin/env stack runghc

-- emit an EOF to terminate the program
-- ctrl + D

main = interact countWords
  where
    countWords input =
      show (length (lines input)) ++ "\n"
#!/usr/bin/env stack runghc

import Data.List (intersperse)
import Data.Char (isSpace)

-- source
-- https://wiki.haskell.org/Simple_Unix_tools

-- impl Python's strip
clean'' = map (f . f)
  where f = reverse . dropWhile isSpace

-- insert blank space at beginning of each line
blank   = map (s ++)
  where s = replicate 8 ' '

main :: IO ()
main = do
      -- double space a file
  let s1 = unwords $ intersperse "" (words "there is a cow")
      -- undo double space
      s2 = unwords $ filter (not.null) (words s1)
  print s1
  print s2
  print $ clean'' ["  there is a cow  "]
  print $ blank ["there is a cow"]

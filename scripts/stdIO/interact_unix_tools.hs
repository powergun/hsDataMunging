#!/usr/bin/env stack runghc

-- source
-- https://wiki.haskell.org/Simple_Unix_tools
-- motivation
-- how to use haskell to write shell tools ??

-- haskell's interact
-- https://stackoverflow.com/questions/16799755/haskell-interact-function
{-
The interact function takes a function of type String->String as 
its argument. The entire input from the standard input device is 
passed to this function as its argument, and the resulting string 
is output on the standard output device.
-}
-- http://www.cantab.net/users/antoni.diller/haskell/units/unit08.html
io f = interact (unlines . f . lines)

main :: IO ()
main = do
  -- test:
  -- ps aux | ./interact.hs
  io (take 2)

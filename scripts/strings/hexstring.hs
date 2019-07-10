#!/usr/bin/env stack runghc

import Numeric
import Data.Bits

-- real world haskell P/161
intToHexstring :: Int -> String
intToHexstring x =
  "\\u"
  ++ replicate (4 - length h) '0'
  ++ h
  where h = showHex x ""

demoIntToHexstring :: IO ()
demoIntToHexstring = do
  print "//// demo intToHexstring"
  print $ intToHexstring 1337
  print $ intToHexstring 31241241

-- P/162
-- use bit manip to support full unicode code point range 
-- 0x10ffff
intToCodePoint :: Int -> String
-- 0xd800
-- 0b_1101_1000_0000_0000
-- 0xdc00
-- 0b_1101_1100_0000_0000
intToCodePoint n = 
  intToHexstring (a + 0xd800) ++ intToHexstring (b + 0xdc00)
  where 
    a = (n `shiftR` 10) .&. 0x3ff 
    b = n .&. 0x3ff

demoIntToCodePoint :: IO ()
demoIntToCodePoint = do
  print $ intToCodePoint 114111
  print $ showHex 114111 ""

main :: IO ()
main = do
  demoIntToHexstring
  demoIntToCodePoint

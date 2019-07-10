#!/usr/bin/env stack runghc

import qualified Data.ByteString.Char8 as B8

demoSplitByChar :: IO ()
demoSplitByChar = do
  print "demo split string by char"
  -- note that break() only works with byte-string
  -- to convert c-string to byte-string, use pack() (O(n))
  print $ B8.break (== '\n') (B8.pack("thereisacow"))
  -- break is split(1) (once)
  print $ B8.break (== '\n') (B8.pack("there\nis\na\ncow"))

main :: IO ()
main = do 
  demoSplitByChar

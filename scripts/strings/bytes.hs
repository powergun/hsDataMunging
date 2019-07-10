#!/usr/bin/env stack runghc

-- haskell design pattern P/81
-- how to pack/unpack char8 
import System.IO
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.Char (chr)

-- pack
-- http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Char8.html
-- O(n) Convert a String into a ByteString
-- For applications with large numbers of string literals, pack 
-- can be a bottleneck.
demoPackChar8 :: IO ()
demoPackChar8 = do
  print "//// demo Char8.pack (string to bytes)"
  print $ B8.pack ""
  print $ B8.pack "CCCaaa32\x345"
  print $ B8.pack "thereis\nacow"

demoConvertBytesToString :: IO ()
demoConvertBytesToString = do
  print "//// demo convert bytes to string"
  print $ toInts $ B8.pack "set iddqd 1"
  print $ toString $ B8.pack "set iddqd  1"
  where
    toInts = map (fromEnum) . B.unpack
    toString = map (chr . fromEnum) . B.unpack
    -- recall String is [Char] (which is also a list)

main :: IO ()
main = do
  demoPackChar8
  demoConvertBytesToString

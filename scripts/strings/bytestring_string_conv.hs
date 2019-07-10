#!/usr/bin/env stack runghc

-- motivation: How to convert string to bytestring (and backward)
{-
see:
https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring

the last answer by robx appears to be the most valuable one
-}

-- Data.Text.Encoding comes from the Haskell Platform
-- Data.ByteString.UTF8.fromString is fine, but requires the 
-- utf8-string package
-- import Data.ByteString.Lazy.UTF8 as BLU
-- import Data.ByteString.UTF8 as BSU

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- to use the lazy version (particularly to implement those 
-- examples in the book, real world haskell), import the following 
-- module as a drop-in replacement
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

demoStringByteStringRoundTrip :: IO ()
demoStringByteStringRoundTrip = do
  print "//// demo string (utf8) to bytestring"
  let _str = "thereis的acow"
      lstr = (TE.encodeUtf8 . T.pack) _str
      lstrLazy = (TLE.encodeUtf8 . TL.pack) _str
  print lstr
  print lstrLazy
  putStrLn $ (T.unpack . TE.decodeUtf8) lstr
  putStrLn $ (TL.unpack . TLE.decodeUtf8) lstrLazy

main :: IO ()
main = do
  demoStringByteStringRoundTrip

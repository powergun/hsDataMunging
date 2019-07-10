#!/usr/bin/env stack runghc

-- real world haskell P/233
-- a string is represented as a list of char values; each elem
-- of a list is allocated individually and has some bookkeeping
-- overhead
-- the bytestring library provides a fast, cheap alternative 
-- to the string type; code written with bytestring can often 
-- match the perf and mem footprint of C
-- the library supplies two modules - each defines functions
-- that are nearly drop-in replacements for their string 
-- counterparts

-- for streaming a large quantity (TB), lazy bytestring type 
-- is usually best. Its chunk size is tunned to be friendly
-- to a modern CPU's L1 cache, and a garbage collector can 
-- quickly discard chunks of streamed data that are no longer 
-- being used

-- the strict bytestring type performs best for applications that 
-- are less concerned with memory footprint or that need to 
-- access data randomly

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Char (isSpace)
import Data.Word

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where
    -- P/235
    -- L.pack function takes a list of Word8 values (Int8)
    -- A Word is an unsigned integral type, with the same size as Int.
    -- see http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Word.html 
    elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElf :: FilePath -> IO Bool
isElf filename = do
  -- P/235
  -- L.readFile is the lazy bytestring equivalent of readFile
  -- it operates lazily, reading the file as data is demanded
  -- it is a good choice for our task: since we only need to 
  -- read at most the first four bytes of the file
  -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- we can safely use this function on a file of any size
  content <- L.readFile filename 
  return (hasElfMagic content)

demoRead4BytesFromFile :: IO ()
demoRead4BytesFromFile = do
  isElf "some.elf" >>= print
  isElf "some.macho" >>= print

demoCreateEmptyBytestring :: IO ()
demoCreateEmptyBytestring = do
  print $ L.empty

demoGetProperties :: IO ()
demoGetProperties = do
  print "//// demo get bytestring properties"
  let bs = (TLE.encodeUtf8 . TL.pack) "     thereisacow 1337"
  print $ L.length bs

demoDropChars :: IO ()
demoDropChars = do
  print "//// demo drop characters"
  let bs = (TLE.encodeUtf8 . TL.pack) "     thereisacow 1337"
  print $ L8.dropWhile isSpace bs  -- remove all leading spaces
  print $ L8.drop 133 bs  -- drop all characters (auto fit)
  print $ L8.drop 7 bs  -- drop 7 leading characters
  -- drop all leading spaces then drop another 5 leading chars
  print $ ((L8.drop 5) . (L8.dropWhile isSpace)) bs

main :: IO ()
main = do
  demoRead4BytesFromFile
  demoCreateEmptyBytestring
  demoGetProperties
  demoDropChars

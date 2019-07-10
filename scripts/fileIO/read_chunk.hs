#!/usr/bin/env stack runghc

import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (chr)

-- P/81
-- if the chunk contains a newline character, then we return
-- the LineEnd function; otherwise it is just another Chunk
-- function

data Chunk = Chunk {chunk :: String}
           | LineEnd {chunk :: String, remainder :: String}
  deriving (Show)

parseChunk chunk = 
  if rightS == B8.pack "" then
    Chunk (toS leftS)
  else
    LineEnd (toS leftS) ((toS . B8.tail) rightS)
  where
    (leftS, rightS) = B8.break (== '\n') chunk
    -- why map (chr . fromEnum) is needed here?
    -- https://stackoverflow.com/questions/4702325/best-way-to-convert-between-char-and-word8
    -- For conversion between Char8 and Word8 you should be able
    -- to use toEnum/fromEnum conversions, as they represent the 
    -- same data.

    -- For Char and Strings you might be able to get away with 
    -- Data.ByteString.Char8.pack/unpack or some sort of combination 
    -- of map, toEnum and fromEnum, but that throws out data if 
    -- you're using anything other than ASCII.

    -- For strings which could contain more than just ASCII a 
    -- popular choice is UTF8 encoding. I like the utf8-string 
    -- package for this:
    toS = map (chr . fromEnum) . B.unpack

demoParseChunk :: IO ()
demoParseChunk = do
  print "//// demo parse chunk"
  print $ (parseChunk (B8.pack "AAA\nBBB"))
  print $ (parseChunk (B8.pack "CCC"))

-- P/82
-- what we call "imperative IO", is strictly called handled-based 
-- IO
-- handle based IO has some good characteristics:
-- processing is incremental (the procssing of a file)
-- NOTE: recall the use of B8.hGet
-- we have precise control over resources 

-- the downsides of handle based IO are
-- IO is expressed at relatively low level of abstraction
-- not very composable; we interleave the iteration of the file
-- with the processing of the chunks
-- NOTE: this also causes trouble for debugging, investigation etc..
-- we have the exposed traversal state: we need to pass the handle
-- around and check for EOF at each iteration; we need to explicitly
-- clean up the resource

demoReadAllBytes :: String -> IO ()
demoReadAllBytes filename = do
  print "//// demo read all bytes"
  fileH <- openFile filename ReadMode
  loop "" fileH
  hClose fileH
  where
    loop acc h = do
      isEOF <- hIsEOF h
      if isEOF then
        print ""
      else do
        chunk <- B.hGet h 8
        -- recall Rust's option-based pattern
        -- Some(n, ..) => {}
        case (parseChunk chunk) of 
          (Chunk chunk') -> do
            let accLine = acc ++ chunk'
            loop accLine h
          (LineEnd chunk' remainder) -> do
            let line = acc ++ chunk'
            putStrLn line
            loop remainder h

main :: IO ()
main = do
  demoParseChunk
  demoReadAllBytes "records.txt"

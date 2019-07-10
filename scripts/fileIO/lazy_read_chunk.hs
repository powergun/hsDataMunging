#!/usr/bin/env stack runghc

import System.IO
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Char (chr)
import Control.Monad

data Chunk = Chunk {chunk :: String}
           | LineEnd {chunk :: String, remainder :: String}
  deriving (Show)

parseChunk chunk =
  if rightS == LB8.pack ("") then
    Chunk (toS leftS)
  else
    LineEnd (toS leftS) (toS rightS) 
  where
    (leftS, rightS) = LB8.break (== '\n') chunk
    toS = map (chr . fromEnum) . LB.unpack

demoParseChunk :: IO ()
demoParseChunk = do
  print "//// demo parse chunk"
  -- expect LineEnd
  print $ (parseChunk (LB8.pack "AAA\nBBB"))
  -- expect Chunk
  print $ (parseChunk (LB8.pack "CCC"))

chunkStream :: Handle -> IO [LB8.ByteString]
chunkStream h = do
  isEof <- hIsEOF h
  if isEof then
    return []
  else do
    chunk <- LB.hGet h 8
    rest <- (chunkStream h)
    return (chunk:rest)

demoChunkStream :: IO ()
demoChunkStream = do
  print "//// demo chunk stream"
  h <- openFile "records.txt" ReadMode
  chunks <- chunkStream h
  print $ take 5 chunks 
  -- [" 9:39  u","p 1 day,"," 21 mins",", 3 user","s, load "]
  --   ^^^^^^^^ 8 chars
  hClose h

processChunk :: [LB8.ByteString] -> IO ()
-- base case: to terminate the recursion
processChunk x = processChunk' "" x

processChunk' :: String -> [LB8.ByteString] -> IO ()
processChunk' acc [] = do print ""
processChunk' acc (chunk:chunks) =
  case (parseChunk chunk) of
    (Chunk chunk') -> do
      processChunk' (acc ++ chunk') chunks
    (LineEnd chunk' remainder) -> do
      let line = acc ++ chunk'
      print line
      processChunk' remainder chunks

-- P/86
-- decoupled a producer from the consumer and the consumer drives 
-- the materializing of the source stream
-- In contrast, in the imperative example, the loop function 
-- drives the iteration through the chunks; Also in the imperative 
-- case the consumer is not explicit
demoProcessChunk :: IO ()
demoProcessChunk = do
  print "//// demo process chunk"
  h <- openFile "records.txt" ReadMode
  chunkStream h >>= processChunk
  hClose h

lineStream accChunks [] = [accChunks]
lineStream accChunks (chunk:chunks) = 
  case (parseChunk chunk) of
    (Chunk chunk') -> 
      lineStream (accChunks ++ chunk') chunks
    (LineEnd chunk' remainder) -> 
      (accChunks ++ chunk') : (lineStream remainder chunks)

demoLineStream :: IO ()
demoLineStream = do
  print "//// demo create line stream from chunk stream"
  h <- openFile "records.txt" ReadMode
  -- lineStream is a pure function of its input stream while
  -- chunkStream is a stream wrapped in an IO monad. This is 
  -- why we need liftM to lift the lineStream function into the
  -- Monad. 
  lines' <- liftM (lineStream "") (chunkStream h)
  -- this also works:
  -- chunks' <- (chunkStream h)
  -- let lines' = lineStream "" chunks'

  -- it is only when we do the mapM method that the stream 
  -- lineStream starts to materialize, which in turn drives the 
  -- evaluation of chunkStream
  -- when we attempt to print one line, the line is materialized
  -- just in time lazily
  -- underneath the mapM_ function, it is the sequence_ function 
  -- that drives the evaluation of the stream
  mapM_ print lines'
  hClose h

main :: IO ()
main = do
  demoParseChunk
  demoChunkStream
  demoProcessChunk
  demoLineStream

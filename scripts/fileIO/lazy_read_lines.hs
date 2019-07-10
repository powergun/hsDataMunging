#!/usr/bin/env stack runghc

import System.IO
import Control.Monad

-- haskell design pattern P/87
-- NOTE: this concludes lazy_read_chunks
-- P/87: 
-- this brings us back full circle to the lines library function, 
-- which lazily returns lines from file chunks

demoLazyReadLines :: IO ()
demoLazyReadLines = do
  h <- openFile "records.txt" ReadMode
  lines' <- (hGetContents h >>= return . lines)
  -- in lazy_read_chunks, it is done this way
  -- lines' <- liftM (lineStream "") (chunkStream h)
  mapM print lines'
  hClose h

-- compose pure functional streams with IO streams using monadic
-- operators and functions
-- in the same way that we can often express functions as pipelines 
-- of simpler functions 
-- many practical IO can be modeled as processing pipelines of 
-- streams

main :: IO ()
main = do
  demoLazyReadLines

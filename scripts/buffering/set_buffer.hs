#!/usr/bin/env stack runghc

-- real world haskell P/230

import System.IO

demoSetBufferMode :: IO ()
demoSetBufferMode = do
  hSetBuffering stdin (BlockBuffering Nothing)
  -- force haskell to flush the buffer
  hFlush stdout
  print 1

main :: IO ()
main = do
  demoSetBufferMode
#!/usr/bin/env stack runghc

import System.Environment (getArgs)

-- real world haskell P/112

interactWith f input output = do
  contents <- readFile input
  writeFile output (f contents)

main = mainWith myFunction 
  -- P/112 the do keyword introduces a block of actions that can 
  -- cause side effects in the real world
  -- the <- opreator is the equivalent of assignment inside a do block
  where mainWith f = do
          args <- getArgs
          case args of
            [input, output] -> interactWith f input output 
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = testFunc
        
testFunc x = x

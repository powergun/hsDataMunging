#!/usr/bin/env stack runghc

import System.IO

-- haskell design pattern P/81

demoPrintAllLines :: String -> IO ()
demoPrintAllLines filename = do
  print "//// demo print all lines"
  h <- openFile filename ReadMode
  loop h
  hClose h
  where
    loop h' = do
      isEof <- hIsEOF h'
      if isEof then
        print ""
      else do
        line <- hGetLine h'
        print $ words line
        loop h'

main :: IO ()
main = do
  demoPrintAllLines "records.txt"

#!/usr/bin/env stack runghc

-- haskell design pattern P/84
-- the first example, keep a list of io-actions, is explained 
-- in monads/lazy_io

import System.IO
import Data.Foldable

demoGetLineActionsInList :: IO ()
demoGetLineActionsInList = do
  print "//// demo call get-line actions from a list"
  h <- openFile "records.txt" ReadMode
  -- line1 function is ready with the first hGetLine h
  line1 <- hGetLine h
  -- line2 and line3 functions are only read when sequenced
  let getLines = [hGetLine h, hGetLine h]
  [line2, line3] <- sequence getLines
  print line1
  hClose h
  print line2

-- P/84
-- while hGetLine returns a strict string, hGetContents return
-- a lazy string
-- hGetContents streams file contents on demand while hGetLine
-- function does not 
demoGetLazyString :: IO ()
demoGetLazyString = do
  print "//// demo get lazy string"
  h <- openFile "records.txt" ReadMode
  contents <- hGetContents h
  -- lazily fetch 3 chars
  print (take 10 contents)
  hClose h

-- P/84
-- using the lines function together with hGetContents function 
-- we get a lazy stream of file lines
demoGetLazyStreamOfLines :: IO ()
demoGetLazyStreamOfLines = do
  print "//// demo get a lazy stream of lines"
  h <- openFile "records.txt" ReadMode
  lines' <- lineStream h
  
  -- SEQUENCING 

  -- P/85
  -- sequence_ is a Prelude function; this works
  -- sequence_ (map putStrLn lines')

  -- P/85
  -- mapM_ function captures the common pattern of mapping and 
  -- sequencing; here mapM_ f is equal to sequence_ (map f)
  mapM_ putStrLn lines'

  hClose h
  where
    lineStream h = hGetContents h >>= return . lines

-- P/85
-- forM_ (require import Data.Foldable)
-- https://stackoverflow.com/questions/6688998/what-is-the-difference-between-form-and-form-in-haskell
-- P/85 forM_ is just a mapM_ function with flipped arguments, which is
-- useful when you want to pass a trailing lambda

-- P/85
-- when performing a lazy/IO, we need to make the distinction
-- between an IO action and performing an IO action.
-- Also we need to know the lazy/strict characteristics of the 
-- functions we are working with (e.g. hGetLine and hGetContents)
demoGetLazyStreamLambda :: IO ()
demoGetLazyStreamLambda = do
  print "//// demo get lazy stream of lines, sequence it with lambda"
  h <- openFile "records.txt" ReadMode
  lines' <- lineStream h
  forM_ lines' $ \line -> do
    let reversed = reverse line
    putStrLn reversed
  hClose h
  where
    lineStream h = hGetContents h >>= return . lines

main :: IO ()
main = do
  demoGetLineActionsInList
  demoGetLazyString
  demoGetLazyStreamOfLines
  demoGetLazyStreamLambda

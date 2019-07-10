#!/usr/bin/env stack runghc
{-# LANGUAGE BangPatterns #-}

-- what is bang pattern
-- https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/bang-patterns.html
-- very good explanation:
-- https://ocharles.org.uk/posts/2014-12-05-bang-patterns.html
-- worth reading his other haskell posts
-- https://ocharles.org.uk/pages/2014-12-01-24-days-of-ghc-extensions.html

module Main where
import System.IO

-- inspired by udemy mastering haskell programming

-- write 100 numbers in a text file, using unlines
demoWriteFile :: IO ()
demoWriteFile = do
  print "//// demo write file"
  writeFile "/var/tmp/sut/records.txt" $
    unlines $
      map show [1..100::Int]

-- real world haskell P/209
-- remember System.IO
demoWriteOneLine :: IO ()
demoWriteOneLine = do
  print "//// demo write one line"
  -- see also P/211
  -- IO mode (r, w, rw, a)
  -- openBinaryFile 
  h <- openFile "/var/tmp/sut/oneline" WriteMode
  hPutStrLn h "thereiascow" 
  -- there is also hPrint()
  hClose h

-- real world haskell P/210
-- return() in haskell is the opposite of <- that is, return
-- takes a pure value and wraps it inside IO
-- since every IO action must return some IO type, if your result
-- came from pure computation, you must use return to wrap it 
-- in IO
-- if 7 is an Int then return 7 would create an action stored in 
-- a value of type IO Int. When executed, that action would 
-- produce the result 7
demoRoundTrip :: IO ()
demoRoundTrip = do
  print "//// demo round trip (read file)"
  -- sum all the first 100 odd numbers (there are only 50 of them)
  h <- openFile "/var/tmp/sut/records.txt" ReadMode
  let go :: Int -> Int -> IO Int
      go 100 !acc = return acc
      go n   !acc = do
        r <- hIsEOF h
        if r then
          return acc
        else do
          x <- read <$> hGetLine h
          go (n + 1) (if odd x then x + acc else acc)
  r <- go 0 0
  print r

main :: IO ()
main = do
  demoWriteFile
  demoRoundTrip
  demoWriteOneLine

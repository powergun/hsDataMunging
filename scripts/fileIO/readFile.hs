#!/usr/bin/env stack runghc

module Main where
import System.IO

demoWriteFile :: IO ()
demoWriteFile = do
  print "//// demo write file"
  writeFile "/var/tmp/sut/records.txt" $
    unlines $
      map show [1..100::Int]

-- explained in udemy mastering haskell programming
-- see: readFile
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:readFile
-- The readFile function reads a file and returns the contents 
-- of the file as a string. 
-- The file is read lazily, on demand, as with getContents.

transform :: String -> Int
transform = sum . filter odd . take 100 . map read . lines

demoRoundTripLazyIO :: IO ()
demoRoundTripLazyIO = do
  print "//// demo round trip (read file, lazy IO)"
  s <- readFile "/var/tmp/sut/records.txt"
  print $ transform s

-- it is better to use withFile to make sure the file handle
-- is closed, even in the case of exception
demoWithFile :: IO ()
demoWithFile = do
  print "//// demo round trip (read file, with-file)"
  withFile "/var/tmp/sut/records.txt" ReadMode $ \h -> do
    s <- hGetContents h
    print $ transform s

main :: IO ()
main = do
  demoWriteFile
  demoRoundTripLazyIO
  demoWithFile

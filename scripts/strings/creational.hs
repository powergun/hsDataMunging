#!/usr/bin/env stack runghc

demoReplicate :: IO ()
demoReplicate = do
  print "//// demo replicate"
  -- for num <= 0, return empty list
  print $ replicate (-1) "asd" 
  -- return a list of 11 elements
  print $ replicate 11 "asd"

main :: IO ()
main = do
  demoReplicate

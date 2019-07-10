#!/usr/bin/env stack runghc

-- udemy master haskell programming

module Main where

source :: [Int]
-- use a simple infinite list to model the real world streaming 
-- problem
source = [0..]

main :: IO ()
main = do
  print 1

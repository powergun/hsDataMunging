#!/usr/bin/env stack runghc

-- real world haskell P/113
--
isSeparator :: Char -> [Char] -> Bool
isSeparator c [] = False
isSeparator c cs = any (== c) cs
isPipe c = c == '|'

demoIsSeparator :: IO ()
demoIsSeparator = do
  print "//// demo isSeparator"
  print $ isSeparator '|' ['\\', ',', '.']
  print $ isSeparator ',' ['\\', ',', '.']

splitLines [] = []
splitLines cs = 
  let (prefix, suffix) = break isPipe cs
  -- real world haskell P/114
  -- The pre : expression tells us that we should add the pre 
  -- value to the front of the list of lines. 
  -- We then use a case expression to inspect the suffix, so we 
  -- can decide what to do next. 
  -- The result of the case expression 
  -- will be used as the second argument to the (:) list constructor.

  -- _______________________ --> produce a new list of strings
  in prefix : case suffix of
    -- can partition the string using double pipe (||) and single
    -- pipe (|), thanks to the first pattern: |:|:rest
    ('|':'|':rest) -> splitLines rest
    ('|':rest) -> splitLines rest
    _ -> []

demoSplitLines :: IO ()
demoSplitLines = do
  print "//// demo splitLines"
  print $ splitLines ""
  print $ splitLines "thereisacow"
  print $ splitLines "there|is|a|cow"
  print $ splitLines "there||is||a||cow"

main :: IO ()
main = do
  demoIsSeparator
  demoSplitLines

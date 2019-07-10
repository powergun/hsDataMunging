#!/usr/bin/env stack runghc

import Prettify

demoDoc :: IO ()
demoDoc = do
  print $ (text "a") <+> (double 2) <+> (char 'a') <+> line

main :: IO ()
main = do
  demoDoc

#!/usr/bin/env stack runghc

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

demoMatchWord :: IO ()
demoMatchWord = do
  print $ parse matchWord "a test parser" "true"
  print $ parse matchWord "a test paser" "123"
  where
    matchWord :: Parser String
    matchWord = string "true"

demoMatchBool :: IO ()
demoMatchBool = do
  print 1

main :: IO ()
main = do
  demoMatchWord
  demoMatchBool


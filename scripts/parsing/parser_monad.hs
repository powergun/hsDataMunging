#!/usr/bin/env stack runghc

import Control.Applicative
import Data.Char

newtype AtomicParser a = AtomicParser {
  runParser :: String -> [(a, String)]
}

parse :: AtomicParser a -> String -> [(a, String)]
parse p s = runParser p s

atom :: AtomicParser Char
atom = AtomicParser $ \s ->
        case s of
          [] -> []
          (x:xs) -> [(x, xs)]

instance Functor AtomicParser where
  fmap f p = AtomicParser $ \s ->
    case (runParser p s) of
      [] -> []
      [(v, out)] -> [(f v, out)]

demoAtomicParser :: IO ()
demoAtomicParser = do
  print $ parse atom ""
  print $ parse atom "iddqd"

-- programming haskell P178
-- MY NOTES:
-- change the parser function, while maintain the same input
-- note how id and toUpper() is "piggybacked" to the original 
-- parser function
-- it does NOT enable sequencing
demoParserAsFunctor :: IO ()
demoParserAsFunctor = do
  print $ parse (fmap id atom) "iddqd"
  print $ parse (fmap toUpper atom) "iddqd"

main :: IO ()
main = do
  demoAtomicParser
  demoParserAsFunctor
  

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

-- P180
-- <*> applies a parser that returns a function to a parser that 
-- returns an argument to give a parser that returns the result
-- of applying the function to the argument, and only succeeds
-- if all the components succeed
-- for example, a parser that consumes three characters, discards 
-- the second and returns the first and third as a pair can now 
-- be defined in applicative style
instance Applicative AtomicParser where
  pure v = AtomicParser (\s -> [(v, s)])
  pf <*> px =
    let newParserFunc s = 
          case (runParser pf s) of
            [] -> []
            [(f, out)] -> runParser (fmap f px) out
    in AtomicParser newParserFunc

demoParserAsApplicative :: IO ()
demoParserAsApplicative = do
  print $ runParser (pure "X") "iddqd"
  let three :: AtomicParser (Char, Char)
      three = pure g <*> atom <*> atom <*> atom
              -- func    arg      arg      arg
              where g x y z = (toUpper x, toUpper z)
  print $ runParser three "iddqd"
  --      ^^^^^^^^^^^^^^^ func arg arg arg 
  -- P180
  -- the applicative machinery automatically ensures that the
  -- above parser fails if the input string is too short, with
  -- the need to detect or manage this ourselves
  print $ runParser three "ad"

-- P180
-- parser p >>= f fails if the application of the parser p
-- to the input string s fails, and otherwise applies the 
-- function f to the result value v to given another parser 
-- f v
-- which is then applied to the output string out that was 
-- produced by the first parser to give the final result 
instance Monad AtomicParser where
  return = pure
  p >>= f =
    let newParserFunc s = 
          case (runParser p s) of
            [] -> []
            [(v, out)] -> runParser (f v) out
    in AtomicParser newParserFunc

-- because AtomicParser is a monadic type, the do notation can
-- now be used to sequence parsers and process their result 
-- values
demoParserMonad :: IO ()
demoParserMonad = do
  print $ runParser (return 'L') "idkfa"
  let three :: AtomicParser (Char, Char)
      three = do
        x <- atom
        atom
        z <- atom
        return (x, z)
  print $ runParser three "thereisnospoon"
  print $ runParser three "th"

main :: IO ()
main = do
  demoAtomicParser
  demoParserAsFunctor
  demoParserAsApplicative
  demoParserMonad
  

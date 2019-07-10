#!/usr/bin/env stack runghc

-- haskell cookbook L2943
-- L3061
-- INI parser is a combinatorial top-down recursive-descent
-- parser.

import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Map hiding (empty)
import Data.Char

type Variables = Map String String
type Sections = Map String Variables
newtype INI = INI Sections deriving (Show)

data Parser a = Parser { runParser :: String -> Maybe (a, String) }
  -- L2943
  -- takes a string as input and generates a tuple, where a
  -- is the type of the value parsed by the parser, 
  -- the second elem represents the remaining input after 
  -- parsing the value of type a
  
-- L2943
-- if we have a parser of type a, then we can define a Functor
-- instance. If we apply a function of type a -> b, we can 
-- convert Parser a into Parser b.
-- MY NOTES:
-- Parser p to Parser parserfunc
-- THIS FUNCTION CAN NOT BE COMPILED UNTIL Monad instance is 
-- fully defined; so that <- makes sense to the compiler
-- MY IMPORTANT NOTES:
-- this is differnt to the example in book where uses let .. in 
-- notation but the code does not compile; using lambda works
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (x, remaining) <- p input -- run parser against input; 
          -- shortcircuit if Nothing
      return (f x, remaining) -- apply func on parsed value

instance Applicative Parser where
  pure x = 
    let parserfunc input = Just (x, input)
    in Parser parserfunc
  -- MY NOTES: 
  -- I like this notation: pf (indicating it wraps a 
  -- function), pa (indicating it wraps a "value" - which is 
  -- actually a function too)
  (Parser pf) <*> (Parser pa) = 
    Parser $ \input -> do
      (f, remaining) <- pf input
      (a, remaining2) <- pa remaining
      return (f a, remaining2)

instance Monad Parser where
  return = pure
  (Parser pa) >>= fab =
    Parser $ \input -> do
      (a, remaining) <- pa input
      runParser (fab a) remaining

-- L2960
-- Alternative is a logical extension of Applicative, where it 
-- allows us to define an "empty" (or complementary to pure) case
-- for our data type, and if we have two values of data types,
-- we can go with the second one if the first one is "empty"
instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  -- MY NOTES:
  -- it works like Perl's or-do
  (Parser pa) <|> (Parser pb) =
    Parser $ \input -> do
      case pa input of
        Nothing -> pb input
        Just (x, remaining) -> Just (x, remaining)
  -- L2974
  -- return a list of v for which v satisfies; the list should
  -- satisfy at least one v
  some v =
    Parser $ \input -> do
      (x,   remaining) <- runParser v input
      (xs, remaining2) <- runParser (many v) remaining
      return (x:xs, remaining2)
  -- L2974
  -- return a list of v for which v satisfies, the list can 
  -- satisfy zero or more v
  -- some() matches at least one value;
  -- whereas many() matches zero or more values parsed by the 
  -- supplied parser
  many v =
    Parser $ \input -> do
      case runParser (some v) input of
        Just (xs, remaining) -> Just (xs, remaining)
        Nothing -> Just([], input)

conditional :: (Char -> Bool) -> Parser Char
conditional f =
  Parser parsefunc
  where
    parsefunc [] = Nothing
    parsefunc (x:xs) | f x = Just (x, xs)
    parsefunc _ = Nothing

char :: Char -> Parser Char
char c = conditional (== c)

bracketed :: Parser a -> Parser b -> Parser c -> Parser b
bracketed pa pb pc = do
  pa
  b <- pb
  pc
  return b

bracketOpen :: Parser Char
bracketOpen = char '['

bracketClose :: Parser Char
bracketClose = char ']'

alphanum :: Parser Char
alphanum = conditional isAlphaNum

isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\t' = True
isWhiteSpace _ = False

whitespace :: Parser Char
whitespace = conditional isWhiteSpace

whitespaces :: Parser String
whitespaces = many whitespace

sectionName :: Parser String
sectionName = bracketed whitespaces (some alphanum) whitespaces

sectionHeader :: Parser String
sectionHeader = bracketed bracketOpen sectionName bracketClose

name :: Parser String
name = (some alphanum)

quote :: Parser Char
quote = char '\"'

quotedchar :: Parser Char
quotedchar = conditional (\c -> isAlphaNum c || isWhiteSpace c)

quotedvalue :: Parser String
quotedvalue = bracketed quote (many quotedchar) quote

value :: Parser String
value = name <|> quotedvalue

assignment :: Parser (String, String)
assignment = do
  whitespaces
  name <- name
  whitespaces
  char '='
  whitespaces
  value <- value
  return (name, value)

newline :: Parser Char
newline = conditional (\c -> c == '\r' || c == '\n' )

newlines :: Parser ()
newlines = many newline >> return ()

blank :: Parser ()
blank = whitespaces >> newline >> return ()

blanks :: Parser ()
blanks = many blank >> return ()

assignments :: Parser Variables
assignments = fromList <$> many (blanks >> assignment)

section :: Parser (String, Variables)
section = do
  blanks
  whitespaces
  name <- sectionHeader
  blanks
  variables <- assignments
  return (name, variables)

ini :: Parser INI
ini = (INI . fromList) <$> many section

main :: IO ()
main = do
  let filename = "simple.ini"
  contents <- readFile (filename)
  case runParser ini contents of
    Just ini_ -> print ini_
    Nothing -> return ()

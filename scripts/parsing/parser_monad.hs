#!/usr/bin/env stack runghc

import Control.Applicative
import Data.Char

-- programming haskell

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

atom2 :: AtomicParser String
atom2 = AtomicParser $ \s ->
          case s of
            (x:y:xs) -> [([x, y], xs)]
            _ -> []

atom3 :: AtomicParser String
atom3 = AtomicParser $ \s ->
          case s of
            (x:y:z:xs) -> [([x, y, z], xs)]
            _ -> []

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

-- P181
-- another natural way of combining parsers is to apply one 
-- parser to the input string, and if this fails to then apply 
-- another to the same input instead
-- P182
-- empty is the parser that always fails regardless of the input
-- and <|> is a choice operator that returns the result of the 
-- first parser if it succeeds on the input, and applies the 
-- second parser to the same input otherwise
instance Alternative AtomicParser where
  empty = AtomicParser (\_ -> [])
  p <|> q = 
    let newParserFunc s = 
          case (runParser p s) of
            [] -> runParser q s
            [(v, out)] -> [(v, out)]
    in AtomicParser newParserFunc

demoParserAsAlternative :: IO ()
demoParserAsAlternative = do
  print $ runParser (empty :: AtomicParser Char) "idnoclip"
  print $ runParser (empty <|> atom) "idn"
  -- atom2 and atom3 discards source string of length less than
  -- 2 and 3
  print $ runParser (empty <|> atom3 <|> atom2) "id"
  print $ runParser (empty <|> atom3 <|> atom2) "idnoclip"

sat :: (Char -> Bool) -> AtomicParser Char
sat p = do
  x <- atom
  if p x
  then
    return x
  else
    empty

-- MY NOTES:
-- THIS IS VERY IMPORTANT! Business logic is born from these 
-- derived primitives!
ident :: AtomicParser String
ident = do 
  x <- lower
  xs <- many alphanum
  return (x:xs)
nat :: AtomicParser Int
nat = do 
  xs <- some digit
  return (read xs) -- awesome!!!
int :: AtomicParser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat
hexstr :: AtomicParser String
hexstr = do 
  string "0x"
  ns <- many alphanum
  return ("0x" ++ ns)
space :: AtomicParser ()
space = do 
  many (sat isSpace)
  return ()
digit :: AtomicParser Char
digit = sat isDigit
lower :: AtomicParser Char
lower = sat isLower
upper :: AtomicParser Char
upper = sat isUpper
letter :: AtomicParser Char
letter = sat isAlpha
alphanum :: AtomicParser Char
alphanum = sat isAlphaNum
char :: Char -> AtomicParser Char
char x = sat (== x)

-- P183
-- using char we can define a prser string xs for the string
-- of characters xs, with the string itself returned as 
-- the result value
-- note that string only succeeds if the entire target is 
-- consumed from the input
string :: String -> AtomicParser String
string [] = return []
string (x:xs) = do
  char x -- trash the result
  string xs -- recursion
  return (x:xs)
demoPredicate :: IO ()
demoPredicate = do
  print $ runParser (char 'X') "Xoiasd"
  print $ runParser (char '_') "0x12"
  print $ runParser lower "IDDQD"
  print $ runParser (string "local") "local value=\"iddqd\""

  -- P184
  -- many p and some p apply a parser p as many times as possible 
  -- until it fails, with the result values from each successful
  -- application of p being returned in a list
  -- the difference between these two repetition primitives is 
  -- that many permits zero or more applications of p, whereas
  --      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- some requires at least one successful application
  -- ^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- the definition of many() and some() are provided in the 
  -- Alternative class (defined using mutual recursion)
  print $ runParser (many digit) "01213 'there is a cow 1337'"
  print $ runParser (many digit) ":address 1234"
  print $ runParser (some digit) "0123 there is 13 a cow"

  print $ runParser ident "const std::string* pStr = nullptr;"
  print $ runParser nat "1234:4567 mov ax, bx"
  print $ runParser int "-1234 "
  print $ runParser hexstr "0x3e7"
  print $ runParser space "    def __init__(self):"

-- P185
-- most real-life parsers allow spacing to be freely used around 
-- the basic tokens in their input string
-- we define a new primitive that ignores any space before and 
-- after applying a parser for a token
token :: AtomicParser a -> AtomicParser a
token p = do 
  space
  v <- p
  space
  return v
identifier :: AtomicParser String
identifier = token ident
natural :: AtomicParser Int
natural = token nat
integer :: AtomicParser Int
integer = token int
symbol :: String -> AtomicParser String
symbol xs = token (string xs)
-- P186
-- for example using these primitives a parser for a non-empty
-- list of natural numbers that ignores spacing around tokens
-- can be defined as:
nats :: AtomicParser [Int]
nats = do 
  symbol "["
  n <- natural
  ns <- many $ do 
    symbol "," 
    natural
  symbol "]"
  return (n:ns)
demoIgnoreSpaceCharacters :: IO ()
demoIgnoreSpaceCharacters = do
  -- MY NOTES:
  -- recall space will drain any continuous space characters
  print $ runParser nats "  [   1  ,   2  ,    3  ,     4  ]  "
  print $ runParser nats "[  ]"
  -- P186 
  -- note that nats only succeeds if a complete list in precisely
  -- this format is consumed
  print $ runParser nats "[1,]"

-- P186
-- the syntactic structure of a language can be formalised using 
-- the mathematical notion of a grammar, which is a set of rules 
-- that describes how strings of the language can be constructed
-- P188
-- have a separate rule for each level of priority, with 
-- addition at the lowest level of priority, mult at the mid
-- level and parentheses and numbers at the highest level
expr :: AtomicParser Int
expr = do {
  t <- term;
  do { 
    symbol "+";
    e <- expr;
    return (t + e);
  } <|> return t;
}
term :: AtomicParser Int
term = do {
  f <- factor;
  do {
    symbol "*";
    t <- term;
    return (f * t);
  } <|> return f;
}
factor :: AtomicParser Int
factor = do {
  symbol "(";
  e <- expr;
  symbol ")";
  return e;
} <|> natural
eval :: String -> Int
eval xs = 
  case (runParser expr xs) of
    [(n, [])] -> n
    [(_, out)] -> error ("unused input: " ++ out)
    [] -> error "invalid input"
demoArithmeticExpressions :: IO ()
demoArithmeticExpressions = do
  print $ eval "2 + 1337 * 1 + (1 + 2)"
  -- print $ eval "xe + we"
  -- print $ eval "2 + 1as"

main :: IO ()
main = do
  demoAtomicParser
  demoParserAsFunctor
  demoParserAsApplicative
  demoParserMonad
  demoParserAsAlternative
  
  demoPredicate
  demoIgnoreSpaceCharacters

  demoArithmeticExpressions

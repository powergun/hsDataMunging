#!/usr/bin/env stack runghc

import           Numeric
import           Text.ParserCombinators.Parsec

-- real world haskell P/433

-- each key-value pair is separated by the & character
-- HTTP spec is unclear about whether a key must have an
-- associated value; need to be able to distringuish between
-- no value and empty value
p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
  -- many1 ~ some; many1 can fail
  name <- many1 p_char
  -- optionMaybe modifies the behavior of a parser
  -- if the parser fails, optionMaybe does not: it returns Nothing
  -- otherwise it wraps the parser's successful result with Just
  -- it gives us the ability to distinguish between "no value"
  -- and "empty value"
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

  -- P/435
  -- compact form:
  -- liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

p_char :: CharParser () Char
p_char =   oneOf urlBaseChars
       <|> (char '+' >> return ' ')
       <|> p_hex

urlBaseChars = ['a'..'z']
            ++ ['A'..'Z']
            ++ ['0'..'9']
            ++ "$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a, b]
  return . toEnum $ d

demoParseURL :: IO ()
demoParseURL = do
  parseTest p_query "foo=bar&a%21=b+c"

main :: IO ()
main = do
  demoParseURL

#!/usr/bin/env stack runghc

import           Control.Monad
import           System.IO
import           Text.ParserCombinators.Parsec

data Element = Empty
             | Comment { comment :: String }
             | Variable { name :: String, value :: String }
             deriving (Eq, Show)

tfvarFile = endBy line eol
line = p_comment <|> p_variable

p_variable :: CharParser () Element
p_variable = do
  name <- many p_char
  value <- (char '=' >> many p_char)
  return Variable { name = name, value = value }

p_comment :: CharParser () Element
p_comment = do
  char '#'
  comment <- many p_char
  return Comment { comment = comment }

p_char = noneOf "\n="

eol = char '\n'

main :: IO ()
main = do
  -- content <- readFile "terraform.tfvars"
  parseTest tfvarFile "#foo variable\nfoo=bar\n#map name\nmap=e1m1\n"
  parseTest tfvarFile "#foo variable\n\nfoo=bar\n#map name\nmap=e1m1\n"

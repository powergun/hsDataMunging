#!/usr/bin/env stack runghc

import           Text.ParserCombinators.Parsec

import           System.IO

tfvarFile = endBy line eol
line = sepBy variable (char '=')
variable = many (noneOf "=\n")
eol = char '\n'

parseTfvars :: String -> Either ParseError [[String]]
parseTfvars input =
  parse tfvarFile "(unknown)" input

main :: IO ()
main = do
  content <- readFile "terraform.tfvars"
  print $ parseTfvars content

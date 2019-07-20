#!/usr/bin/env stack runghc

-- real world haskell P/423
-- regex are nice for many tasks but they rapidly become unwieldly
-- or can not be used at all, when dealing with a complex data
-- format

-- Parsec is a useful parser combinator library, with which we
-- combine small parsing functions to build more sophisticated
-- parsers.

-- Parsing is sometimes divided into two stages:
-- lexical analysis (the domain of tools such as flex)
-- parsing itself (performed by programs such as bison)
-- Parsec can perform both lexical analysis and parsing

import           Text.ParserCombinators.Parsec

import           System.IO
-- a csv file contains 0 or more lines, each of which is
-- terminated by the EOF character
csvFile :: GenParser Char st [[String]]
csvFile = do
  result <- many line
  eof
  return result

-- each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = do
  result <- cells
  eol
  return result

-- build up a list of cells,
-- try to parse the first cell, then figure out what ends the cell
cells :: GenParser Char set [String]
cells = do
  first <- cellContent
  next <- remainingCells
  return (first : next)

-- the cell either ends with a comma, indicating that 1 or more
-- cells follow, or it doesn't, indicating that we are at the end
-- of the cells for this line
remainingCells :: GenParser Char set [String]
remainingCells =
  (char ',' >> cells)
  <|> (return [])

-- each cell contains 0 or more characters, which must not be a
-- comma or EOL
cellContent :: GenParser Char st String
cellContent =
  many (noneOf ",\n")

eol :: GenParser Char st Char
eol =
  char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input =
  parse csvFile "(unknown)" input

demoParseCSV :: IO ()
demoParseCSV = do
  -- empty
  -- print $ parseCSV ""

  -- bad
  -- print $ parseCSV "a"

  -- singleton [["a"]]
  -- print $ parseCSV "a\n"

  -- simple lines
  -- print $ parseCSV "line\nline\nline\n"

  -- empty line
  -- print $ parseCSV "line\n\nline\n"

  content <- readFile "C2ImportCalEventSample.csv"
  print $ parseCSV content

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- real world haskell P/426
-- simplify the above parsing logics

-- sepBy:
-- takes two functions as arguments: the first parses some sort
-- of content, while the second parses a separator.
-- starts by trying to parse content and then separators, and
-- alternates back and forth until it can't parse a separator
-- it returns a list of all the content that it was able to
-- parse

-- endBy:
-- similar to sepBy but expects the very last item to be followed
-- by the separator. That is, it continues parsing until it
-- can't parse any more content

-- we can use endBy to parse lines, since every line must end
-- with the end of line character
-- we can use sepBy to parse cells, since the last cell will not
-- end with a comma
csvFile' = endBy line' eol'
line' = sepBy cell' (char ',')
cell' = many (noneOf ",\n")
eol' = char '\n'

demoParseCSV' :: IO ()
demoParseCSV' = do
  content <- readFile "C2ImportCalEventSample.csv"
  print $ parse csvFile' "(unknown)" content

main :: IO ()
main = do
  demoParseCSV
  demoParseCSV'

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

main :: IO ()
main = do
  -- empty
  -- print $ parseCSV ""

  -- bad
  -- print $ parseCSV "a"

  -- singleton [["a"]]
  -- print $ parseCSV "a\n"

  content <- readFile "C2ImportCalEventSample.csv"
  print $ parseCSV content

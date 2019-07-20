#!/usr/bin/env stack runghc

-- real world haskell P/431
-- a csv parser that can deal with comma and quote in the cell

import           Control.Monad
import           Data.List
import           System.IO
import           Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

-- P/432
-- begins and ends with a quote mark and contains zero or more
-- characters
-- these characters can not be copied directly, though because
-- they may contain embedded, doubled up quote marks themselves
-- call quotedChar()
quotedCell = do
  char '"'
  content <- many quotedChar
  char '"' <?> "quote at the end of cell"
  return content

-- when processing characters inside a quoted cell, we first say
-- noneOf "\"", this will match and return any single character
-- as long as it is not the quote mark
-- otherwise if it is the quote mark, we see if we have two in
-- a row, if so we return a single quote mark to go on our result
-- string
quotedChar = do
  noneOf "\""
  <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input =
  parse csvFile "(unknown)" input

main :: IO ()
main = do
  content <- readFile "Addresses.csv"
  let Right rows = parseCSV content
  forM_ rows $ \r -> do
    putStrLn $ intercalate "|    |" r

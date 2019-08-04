module Parser
  ( parseSed
  ) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Command

parseSed :: String -> [Command]
parseSed s =
  [Substitute "[0-9]+" "classified(\\0)" "", Print]

parsePrint :: Parser Command
parsePrint = do
  _ <- char 'p'
  return Print

parseDelete :: Parser Command
parseDelete = do
  _ <- char 'd'
  return Delete

parseNext :: Parser Command
parseNext = do
  _ <- char 'n'
  return Next

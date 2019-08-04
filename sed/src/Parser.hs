module Parser
  ( parseSed
  ) where

import           Data.Char          (isAlpha)
import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Command

parseSed :: String -> [Command]
parseSed s =
  case parse parseCommands [] s of
    Right cmds -> cmds
    Left err   -> undefined

parseCommands :: Parser [Command]
parseCommands =
  sepEndBy parseCommand (char ';')

parseCommand :: Parser Command
parseCommand = do
  many (char ' ')
  cmd <- parsePrint
        <|> parseDelete
        <|> parseNext
        <|> parseSubstitute
  many (char ' ')
  return cmd

parsePrint :: Parser Command
parsePrint = do
  char 'p'
  return Print

parseDelete :: Parser Command
parseDelete = do
  char 'd'
  return Delete

parseNext :: Parser Command
parseNext = do
  char 'n'
  return Next

parseSubstitute :: Parser Command
parseSubstitute = do
  -- s/pattern/replace/flags*
  char 's'
  -- TODO: allow user-defined delimiter characters (define a custom type)
  delim <- char '/'
  pat <- many (noneOf "/")
  char '/'
  repl <- many (noneOf "/")
  char '/'
  flags <- many (satisfy isAlpha)
  return (Substitute pat repl flags)

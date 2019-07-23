#!/usr/bin/env stack runghc

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative (some)

data JSONValue = B Bool
               | S String
               | N Int
               deriving (Show, Eq)

despace :: Parser a -> Parser a
despace p = 
  many (char ' ') *> p <* many (char ' ')

boolParser :: Parser Bool
boolParser = 
  (string "true" >> return True) 
  <|> (string "false" >> return False)

jsonBool :: Parser JSONValue
jsonBool = do
  b <- boolParser
  return (B b)

stringParser :: Parser String
stringParser = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return s

jsonString :: Parser JSONValue
jsonString = do
  s <- stringParser
  return (S s)

intParser :: Parser Int
intParser = do
  s <- some (oneOf ['0'..'9'])
  return (read s :: Int)

floatParser :: Parser Float
floatParser = do
  d <- some (oneOf ['0'..'9'])
  char '.'
  f <- some (oneOf ['0'..'9'])
  return (read (d ++ ['.'] ++ f) :: Float)

jsonValue =
  jsonBool <|> jsonString

jsonArray :: Parser [JSONValue]
jsonArray = do
  despace (char '[')
  arr <- (despace jsonValue) `sepBy` (char ',')
  despace (char ']')
  return arr

demoString :: IO ()
demoString = do
  print $ parse boolParser "..." "false"
  print $ parse jsonBool "..." "true"
  print $ parse stringParser "..." "\"there is a cow\""
  print $ parse jsonString "..." "\"there is\""
  print $ parse jsonArray "..." " [ \" there \" , \" is \" , true , \" cow \" ] "

demoNum :: IO ()
demoNum = do
  print $ parse intParser "..." "1337"

demoFloat :: IO ()
demoFloat = do
  print $ parse floatParser "..." "13.37"

main :: IO ()
main = do
  demoString
  demoNum
  demoFloat


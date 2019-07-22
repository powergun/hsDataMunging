#!/usr/bin/env stack runghc

-- source 
-- parse stuff in haskell (meetup video)

import Text.ParserCombinators.Parsec
-- import Control.Applicative
-- import Control.Monad

demoMatchWord :: IO ()
demoMatchWord = do
  print $ parse matchWord "a test parser" "true"
  print $ parse matchWord "a test paser" "123"
  where
    matchWord :: Parser String
    matchWord = string "true"

demoMatchTrue :: IO ()
demoMatchTrue = do
  let err = "failed"
      parser = string "true"
  print $ parse parser err "true"
  print $ parse parser err "false"

demoAlwaysTrue :: IO ()
demoAlwaysTrue = do
  let parser = pure True :: Parser Bool
      parser' = string "true"
  print $ parse (parser' *> parser) "..." "iddqd"
  -- if the first succeeds, carry on to the next parser
  print $ parse (parser' *> parser) "..." "true"

demoMatchBool :: IO ()
demoMatchBool = do
  let parser = ((string "true") *> (pure True))
               <|> ((string "false") *> (pure False))
  print $ parse parser "..." "true"
  print $ parse parser "..." "false"
  print $ parse parser "..." "123"

demoMatchStringLiteral :: IO ()
demoMatchStringLiteral = do
  -- interesting syntax!!
  -- the definition of <* is even more entertaining!!
  let parser = (char '"') *> (many (noneOf "\"")) <* (char '"')
  print $ parse parser "..." "thereisacow"
  print $ parse parser "..." "\"there is a cow + 1\""

main :: IO ()
main = do
  demoMatchStringLiteral

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

data JSONValue = B Bool 
               | S String
               deriving (Eq, Show)

demoJavaScriptValue :: IO ()
demoJavaScriptValue = do
  -- note how it uses fmap to wrap the parser's return value
  -- it is a common pattern in Parsec
  -- the example in the video is nicer in that it separates 
  -- the raw parsers that return Haskell's builtin types, from
  -- json parsers that return the JSONValue 
  -- jsonBool = fmap B boolParser
  -- jsonString = fmap S stringParser
  let str :: Parser JSONValue
      str = S <$> ((char '"') *> (many (noneOf "\"")) <* (char '"'))
      boo :: Parser JSONValue
      boo = B <$> (((string "true") *> (pure True)) 
            <|> ((string "false") *> (pure False)))
      jv :: Parser JSONValue
      jv = str <|> boo

      -- borrowed from real world haskell book
      despace :: Parser a -> Parser a
      despace p = many (char ' ') *> p <* many (char ' ')

      -- the arr definition is quite nice
      arr = (despace (char '[')) *>
            ((despace jv) `sepBy` (char ','))
            <* (despace (char ']'))
  
  -- I  can use monadic notation (do notation) instead of *> <* !!
  -- the *> expression can not retrieve the parser's return 
  -- value (not in monadic context)
  print $ parse jv "..." "true" 
  print $ parse (despace jv) "..." "  \" there is a cow \"  "
  print $ parse jv "..." "thereis"
  print $ parse arr "..." "[\"there\", \"is\", true, \"cow\"]"
  print $ parse arr "..." " [ \" there \" , \" is \" , true , \" cow \" ] "

main :: IO ()
main = do
  demoJavaScriptValue

{-# LANGUAGE ScopedTypeVariables #-}

module GlobRegex
  ( demo
  , matchesGlob
  ) where

import Text.Regex.PCRE ((=~))
import Control.Exception (catch, SomeException)

globToRegex :: String -> String
globToRegex cs =
  '^' : globToRegex' cs ++ "$"
globToRegex' :: String -> String
globToRegex' "" = "" 
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = "." ++ globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

-- helper function that only checks that a character class is 
-- correctly terminated
-- it passes its input through unmodified until it hits a ']',
-- when it hands control back to globToRegex' (the caller)
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass [] = error "unterminated character class"

-- real world haskell P/243
-- the escape function ensures that the regexp engine will not
-- interpret certain characters as pieces of regular expression
-- syntax
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
  where
    regexChars = "\\+()^$.{}]|"

demoGlobToRegex :: IO ()
demoGlobToRegex = do
  print "//// demo glob to regex"
  -- expect error thrown
  catch ((return $ globToRegex "*as?d.[POB") >>= print) 
        (\(e :: SomeException) -> print e)
  print $ globToRegex "*as?d.[POB]"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

demoMatchesGlob :: IO ()
demoMatchesGlob = do
  print "//// demo matchesGlob()"
  print $ matchesGlob "iddqd" "idd[a-z][a-z]"

demo :: IO ()
demo = do
  demoGlobToRegex
  demoMatchesGlob

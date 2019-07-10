module DemoPosix
  ( demo
  ) where

-- real world haskell P/238
-- NOTE: Text.Regex.Posix comes from regex-posix package;
-- regex-posix package is not included in haskell platform
-- see: https://stackoverflow.com/questions/29563020/could-not-find-module-text-regex-posix
-- I can not run this in scripting mode; instead I need to create 
-- stack project and update the dependencies section
import Text.Regex.Posix

-- the =~ operator uses typeclasses for both of its arguments 
-- and also for its return type
-- the first arg is the text to match; the second is the regular 
-- expression to match against
-- we can pass either a String or a ByteString as argument

-- if we omit a specific type for the result we will get an 
-- error from the interpreter
-- we tell it what we'd like the type to be; if we want a result 
-- of type bool, we'll get a pass/fail answer
-- NOTE: recall in Perl, I can run the match statement in different
-- contexts - scalar, which gives me the matched char position;
-- list, which gives me the groups of matched sub strings
demoExplicitReturnType :: IO ()
demoExplicitReturnType = do
  print "//// demo explicit return type"
  -- yes or no answer
  (return ("seta thereisacow=1" =~ "^set[a]?\\s*[a-z]" :: Bool)) >>=
    print
  -- P/239
  -- will get the first substring that matches 
  (return ("seta thereisacow=1" =~ "^set[a]?[ ]*[a-z]+" :: String)) >>=
    print
  -- or an empty string if nothing matches
  (return ("seta thereisacow=1" =~ "[2-9]+" :: String)) >>=
    print
  -- return the number of matches (4)
  (return ("there is a cow" =~ "[a-z]+" :: Int)) >>=
    print

demo :: IO ()
demo = do
  demoExplicitReturnType

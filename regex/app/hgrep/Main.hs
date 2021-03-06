
module Main where

import           Control.Monad      (when)
import           System.Environment (getArgs)
import           Text.Regex.PCRE    ((=~))

process :: String -> String -> String
process regex =
  unlines . (filter (\l -> l =~ regex :: Bool)) . lines

{-
test:
hgrep
who | hgrep "weining"
who | hgrep "con\\w+"
who | hgrep "config"
who | hgrep "\\bJ"
-}
main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) $ do
    putStrLn $ "Incorrect arguments " ++ show args
    error "Provide args"
  interact (process (head args))

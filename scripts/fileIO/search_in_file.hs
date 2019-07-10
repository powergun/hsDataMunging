#!/usr/bin/env stack runghc

-- real world haskell P/340
-- search in /etc/passwd

import Data.List
import System.IO
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Usage: prog filename uid"
    exitFailure
  content <- readFile (args !! 0)
  let username = findByUID content (read (args !! 1))
  case username of
    Just x -> putStrLn x
    Nothing -> do 
      putStrLn "not found"
      exitFailure

findByUID :: String -> Integer -> Maybe String
findByUID content uid =
  let lns = filter (\l -> (length l > 0) && (l !! 0) /= '#') (lines content)
      al = map parseline lns
  in lookup uid al

parseline :: String -> (Integer, String)
parseline input =
  let fields = split ':' input
  in (read (fields !! 2), fields !! 0)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim str = 
  let (before, remainder) = span (/= delim) str
  in before : case remainder of
                [] -> []
                x -> split delim (tail x)


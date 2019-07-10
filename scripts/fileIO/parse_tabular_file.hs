#!/usr/bin/env stack runghc

-- this is an improved version over search_in_file
-- real world haskell P/345
-- 

import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Printf (printf)
import System.Environment (getArgs)
import System.Exit
import Control.Monad (when, forM_)

data PasswdEntry = PasswdEntry {
  userName :: String,
  password :: String,
  uid :: Integer,
  gid :: Integer,
  gecos :: String,
  homeDir :: String,
  shell :: String 
} deriving (Eq, Ord)

instance Show PasswdEntry where
  show pe = printf "<%s:%s:%d:%d:%s:%s:%s>"
              (userName pe) (password pe) (uid pe) (gid pe)
              (gecos pe) (homeDir pe) (shell pe)

instance Read PasswdEntry where
  -- TODO: what is readsPrec? what is _? why return [[a, []]]?
  readsPrec _ value = 
    case split ':' value of
      [f1, f2, f3, f4, f5, f6, f7] ->
        [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
      x -> error $ "Invalid: " ++ show x
    where
      split :: Eq a => a -> [a] -> [[a]]
      split _ [] = [[]]
      split delim str =
        let (before, remainder) = span (/= delim) str
        in before : case remainder of
                      [] -> []
                      x -> split delim (tail x)

-- synonym
type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    putStrLn "Usage: prog filename"
    exitFailure
  content <- readFile (head args)
  let lns = filter (\l -> length l > 0 && head l /= '#') (lines content)
      (uidmap, usermap) = inputToMaps lns
  -- printRecords lns
  print $ Map.lookup "root" usermap 
  print $ Map.lookup 230 uidmap
  print $ Map.lookup 2432 uidmap
  return ()

printRecords :: [String] -> IO ()
printRecords lns = do
  -- recall the difference between forM and mapM
  -- and the difference between its "return-value" version and
  -- non-return-value version
  -- HINT: both works with pure list
  forM_ lns $ \l -> do
    -- why `read l` doesn't work?
    -- https://stackoverflow.com/questions/27947925/haskell-prelude-read-no-parse-string
    -- the example in the book P/346 does not use scoped type
    -- variable to give compiler hints on the returning type
    -- I wonder if it was only possible in the old version of ghc
    putStrLn $ show ((read l) :: PasswdEntry)

-- real world haskell P/347
-- this example maintains two maps: one from username to PasswdEntry
-- and another one from UID to PasswdEntry. 
-- Database developers may find it convenient to think of this as 
-- having two difference indices into the data to speed searching
-- on different fields
inputToMaps :: [String] -> (UIDMap, UserMap)
inputToMaps lns =
  (uidmap, usermap)
  where
    uidmap = Map.fromList . map (\pe -> (uid pe, pe)) $ entries
    usermap = Map.fromList . map (\pe -> (userName pe, pe)) $ entries
    entries = map read lns

{-# LANGUAGE ScopedTypeVariables #-}

module Glob
  ( demo

  ) where

-- real world haskell P/246
-- FilePath abstracts the details of an OS path name conventions
-- (i.e. to / or to \)
import System.FilePath ( dropTrailingPathSeparator
                       , splitFileName, (</>))
import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        , getCurrentDirectory
                        , getDirectoryContents)
import Control.Exception (handle, SomeException)
import Control.Monad (forM)
import qualified GlobRegex

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  -- function guard syntax
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    -- return a singleton list if file exists
    return (if exists then [pat] else [])
  | otherwise = do 
    case splitFileName pat of
      -- filename does not have dirname segment
      ("", basename) -> do
        curDir <- getCurrentDirectory
        -- expand '.' to pwd
        listMatches curDir basename
      -- need to process the dirname segment, recursively
      (dirname, basename) -> do
        -- a/b/c
        -- curDir/a/b/c ?
        -- curDir/a/b ?
        -- curDir/a ?
        dirs <- if isPattern dirname
                then namesMatching (dropTrailingPathSeparator dirname)
                else return [dirname]
        let listdir = if isPattern basename
                      then listMatches
                      else listPlain
        -- forM: map its second argument (an action), over its
        -- first (list) and returns the list of the results
        -- RECALL forM_ will drop the results
        pathnames <- forM dirs $ \dir -> do
                        basenames <- listdir dir basename
                        --  partial: lhs op .. 
                        return (map (dir </>) basenames)
        return (concat pathnames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  -- if fileExists
  --   then return True
  --   else doesDirectoryExist name
  case fileExists of
    True -> return True
    False -> doesDirectoryExist name

-- returns a list of all files matching the given glob pattern 
-- in a directory
listMatches :: FilePath -> String -> IO [String]
listMatches dirname pat = do
  -- null can test whether a given string is empty
  -- Prelude> null ""
  -- True
  dirname' <- if null dirname
              then getCurrentDirectory
              else return dirname
  -- real world haskell P/249
  -- handle takes two args, the first is a function that is passed 
  -- an execption value and can have side effects, this is the 
  -- handler to run if an exception is thrown;
  -- the second argument is the code that might throw an exception
  handle (\(e :: SomeException) -> return []) $ do
  -- handle has the same strict requirement of the exception type
  -- as catch() does; see error/catch (also filesystem/tempfile_with)
  -- for more details
    names <- getDirectoryContents dirname'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter (`GlobRegex.matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirname basename = do
  exists <- if null basename
            then doesDirectoryExist dirname
            else doesNameExist (dirname </> basename)
  return (if exists then [basename] else [])

demoNamesMatching :: IO ()
demoNamesMatching = do
  print "//// demo namesMatching()"
  -- hardcoded filename
  namesMatching "package.yaml" >>= print
  -- hardcoded, does not exist
  namesMatching "thereisacow" >>= print
  -- glob
  namesMatching "*.yaml" >>= print
  -- glob, not found
  namesMatching "*.mp4" >>= print
  -- match a lot of stuffs (but only print the first 6)
  -- note this version does not support */**
  -- P/250
  -- ; **.c would mean "match a name ending in .c in this directory
  -- or any subdirectory at any depth"
  filenames <- namesMatching "../*/*/*.hs"
  print $ take 6 filenames

demo :: IO ()
demo = do
  demoNamesMatching


#!/usr/bin/env stack runghc

-- haskell cookbook L2833
-- L2873
-- IO monad is the gateway for pure Haskell function to the 
-- outside world
-- L2882
-- since the IO operations are imperative, the first step is 
-- executed before the following ones.

import System.IO ( hGetLine
                 , hIsEOF
                 , withFile
                 , Handle
                 , IOMode(..)
                 , hGetContents)
import System.Environment (getArgs)
import Control.Monad
import Data.List (intercalate)

getLinesSeq :: Handle -> IO [String]
getLinesSeq h = do
  eof <- hIsEOF h
  if eof then
    return []
  else
    -- L2890
    -- we need to put the single line ahead of the rest of the 
    -- lines returned by getLinesSeq. For the pure list we can
    -- achieve it by (:) function
    -- MY NOTES:
    -- if (hGetLine h) returns string s
    -- this results in f, which is wrapped in IO; therefore
    -- <*> works naturally
    -- (:) <$> hGetLine h <*> getLinesSeq h
    do line <- hGetLine h
       lines <- getLinesSeq h
       return (line : lines)

printLine :: (Int, String) -> IO ()
printLine (lineno, line) =
  putStrLn $ intercalate " : " [show lineno, line]

withLineNumbers :: Monad m => m [String] -> m [(Int, String)]
withLineNumbers m = 
  -- L2890
  -- it takes a monad (any) that represents a list of Strings
  -- lists the function zip() to the monad
  -- it again uses the Functor/Applicative pattern
  --  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- zip <$> pure [1..] <*> m
  do let linenums = [1..] 
     lines <- m -- input monad represents a list of lines
     return (zip linenums lines)

demoPrintLinesWithLineno :: IO ()
demoPrintLinesWithLineno = do
  withFile ("text") ReadMode (\h -> do
    lines <- withLineNumbers (getLinesSeq h)
    forM_ lines printLine
    )
  -- MY NOTES:
  -- this is my modification that is not using custom function, 
  -- and is lazy
  withFile ("text") ReadMode (\h -> do
    lines' <- (hGetContents h >>= return . lines)
    forM_ (zipWith (\n l -> show n ++ " | " ++ l) [1..] lines') putStrLn
    )

main :: IO ()
main = do
  demoPrintLinesWithLineno

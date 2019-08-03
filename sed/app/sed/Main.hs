module Main where

import           Control.Monad      (when)
import qualified Data.Text.IO       as TIO
import           SedLib             (Command (..), sed)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

main :: IO ()
-- Prelude interact accepts String -> String function, therefore
-- I need to use the TIO version of interact
-- test case:
-- Substitute "[0-9]+" "classified(\\0)"
-- py -c "[print('there is %d some' % id(n)) for n in range(10)]" | hsed
main = do
  args <- getArgs
  when (null args) $ do
    putStrLn "Usage: hsed [args...]"
    exitFailure

  TIO.interact $ sed (head args)


module Main where

import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) $ do
    putStrLn $ "Incorrect arguments " ++ (show args)
    error "Provide args"
  print $ args

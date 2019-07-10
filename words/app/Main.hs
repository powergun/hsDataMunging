module Main where

import Lib
import Data

main :: IO ()
main = do
    putStrLn $ show (findWords grid languages)

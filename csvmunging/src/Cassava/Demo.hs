{-# LANGUAGE ScopedTypeVariables #-}

module Cassava.Demo (demo) where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Vector          as V

demo :: IO ()
demo = do
    csvData <- BL.readFile "./testdata/salaries.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (name, salary :: Int) ->
            putStrLn $ name ++ " earns " ++ show salary ++ " dollars"

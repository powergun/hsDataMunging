module Main where

import qualified Barcode

main :: IO ()
main = do
  Barcode.runTests

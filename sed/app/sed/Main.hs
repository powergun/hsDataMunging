module Main where

import qualified Data.Text.IO as TIO
import           SedLib       (Command (..), sed)

main :: IO ()
-- Prelude interact accepts String -> String function, therefore
-- I need to use the TIO version of interact
-- test case:
-- Substitute "[0-9]+" "classified(\\0)"
-- py -c "[print('there is %d some' % id(n)) for n in range(10)]" | hsed
main =
  TIO.interact $ sed [Substitute "[0-9]+" "classified(\\0)" "", Print]

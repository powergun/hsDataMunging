#!/usr/bin/env stack runghc

-- programming haskell L2673

import Data.Char

demoOrd :: IO ()
demoOrd = do
    print $ map ord "asd"

main :: IO ()
main = do
    demoOrd

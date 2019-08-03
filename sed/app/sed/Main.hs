module Main where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- source
-- sed implementation in haskell video

-- reason for using Text type: Text allows utf-8 encoding, whereas
-- String is ascii
sed :: T.Text -> T.Text
sed t = t

main :: IO ()
-- Prelude interact accepts String -> String function, therefore
-- I need to use the TIO version of interact
main = TIO.interact sed

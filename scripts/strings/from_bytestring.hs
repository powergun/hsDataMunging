#!/usr/bin/env stack runghc

-- real world haskell P/236
-- see also: 
-- data_munging/strings/from_string.hs
-- data_munging/csv/ingest.hs (real world haskell book example)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy.Char8 as L

toBS = (TLE.encodeUtf8 . TL.pack)

tryReadInt :: String -> IO ()
tryReadInt s = do
  -- L.readInt returns both the integer and the remainder of the 
  -- string once a run of digits is consumed
  case L.readInt $ toBS s of
    Nothing -> return ()
    Just (v, rest) -> print v

demoReadInt :: IO ()
demoReadInt = do
  print "//// demo lexical_cast int"
  tryReadInt "as"
  tryReadInt "1ax2"  -- NOTE only 1 gets used for the casting
  tryReadInt "3242423"  -- entire string is used for the casting
  tryReadInt "_123123"
  -- this pattern leads the implementation of the recursive
  -- parsing logic demoed in the haskell book (parse dollar and
  -- cents)

main :: IO ()
main = do
  demoReadInt

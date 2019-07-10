#!/usr/bin/env stack runghc

-- real world haskell P/235
-- the character-oriented bytestring modules provide useful
-- functions for text prodessing

import qualified System.IO as IO
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

demoReadAsPlainText :: IO ()
demoReadAsPlainText = do
  print "//// demo read as plain text"
  putStr =<< IO.readFile "prices.csv"
  print ""

demoExtractColumn :: IO ()
demoExtractColumn = do
  print "//// extract price column"
  let str = "2008-08-01,20.09,20.12,19.53,19.80,19777000,19.80"
      --              0     1     2     3     4
      lstr = (TLE.encodeUtf8 . TL.pack) str
  case closing lstr of
    Nothing -> return ()
    Just (price) -> print price

-- see container/list.sh for bang-bang syntax (get element by index)
closing :: L.ByteString -> Maybe Int
closing = readPrice . (!!4) . L.split ','

-- turns a string representing a fractional price into a whole number
readPrice :: L.ByteString -> Maybe Int
readPrice str = 
  case L.readInt str of
    Nothing -> Nothing
    Just (dollars, rest) ->
      case L.readInt (L.tail rest) of
        Nothing -> Nothing
        Just (cents, more) ->
          Just (dollars * 100 + cents)

highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom path = do
  contents <- L.readFile path
  return (highestClose contents)

demoHighestCloseFrom :: IO ()
demoHighestCloseFrom = do
  print "//// demo extract column from all rows and summarise"
  price <- highestCloseFrom "prices.csv"
  case price of
    Nothing -> return ()
    Just v -> print v

main :: IO ()
main = do
  demoReadAsPlainText
  demoExtractColumn
  demoHighestCloseFrom

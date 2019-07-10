#!/usr/bin/env stack runghc

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

demoString :: IO ()
demoString = do
  print $ string "there is acow"

main :: IO ()
main = do
  print 1
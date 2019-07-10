#!/usr/bin/env stack runghc

-- real world haskell P/184
demoNaiveSerializationRoundTrip :: IO ()
demoNaiveSerializationRoundTrip = do
  writeFile "/var/tmp/sut/tt.txt" (show ([Just 5, Nothing, Nothing, Just 8, Just 9]::[Maybe Int]))
  input <- readFile "/var/tmp/sut/tt.txt"
  print ((read input)::[Maybe Int])

main :: IO ()
main = do
  demoNaiveSerializationRoundTrip

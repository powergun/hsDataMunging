#!/usr/bin/env stack runghc

main :: IO ()
main = do
    -- |<-: from ...
    content <- readFile "./hexnums.txt"
    -- |print vs putStrLn
    -- |print is equivalent to {:?} formatter in rust,
    -- |repr() in python and p in ruby; it prints to stdout
    -- |the representation of the argument
    -- |putStrLn adds newline and prints the to_string value of the
    -- |argument
    print content

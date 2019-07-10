#!/usr/bin/env stack runghc

-- |readInts has a signature, that takes a String and produces a list
-- |of Ints
readInts :: String -> [Int]
-- |readInts has 'let' binding which introduces a new variable ws
readInts s =
    let ws = words s in map read ws

minMax :: Ord a => [a] -> Maybe (a, a)
-- |h, t: head and tail
-- |this is to handle the case where the list is NOT empty
minMax (h : t) = Just $ foldr
    -- |this is an anonymouse function, denoted by \ (lambda character)
    (\x (min, max) -> (
        if x < min then
            x
        else
            min
        ,
        if x > max then
            x
        else
            max
        )
    )
    (h, h)
    t
-- |this is to handle the case where the list IS empty
minMax _ = Nothing

main :: IO ()
main = do
    content <- readFile "./hexnums.txt"
    -- |name introduced by let, lives till the end of the do block
    let values = readInts content
        count = length values
        total = sum values
        mean = fromIntegral total / fromIntegral count
        range = minMax values
    print count
    print total
    print mean
    print range

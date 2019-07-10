module Lib
    ( findWord
    , findWords
    , findWordInLine
    ) where

import Data.List
    ( intercalate
    , isInfixOf
    , transpose
    )
import Data.Maybe
    ( catMaybes
    )

someFunc :: IO ()
someFunc = putStrLn someString

someString :: String
someString = "someString"

type Grid = [String]

-- printGrid :: IO ()
-- this would do too
-- printGrid = putStrLn $ intercalate "\n" grid
-- printGrid = putStrLn $ unlines grid

findWord :: Grid -> String -> Maybe String
-- to account for words in the reverse order
findWord grid word =
    let lines = getLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

-- see container/list
skew [] = []
skew (l:ls) =
    let indent line = '_' : line
    in l : skew (map indent ls)

-- see container/list
getLines :: Grid -> [String]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagnoalize grid
        diagonal2 = diagnoalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)

diagnoalize :: Grid -> Grid
-- prefer function composition syntax, and can remove redundant
-- argument from both sides of the equation
-- f x = (g . h) x ==> f = g . h
-- diagnoalize grid = transpose (skew grid)
diagnoalize = transpose . skew

findWords :: Grid -> [String] -> [String]
findWords grid words =
    let wordsFound = map (findWord grid) words
    in catMaybes wordsFound

findWordInLine :: String -> String -> Bool
-- `` allows a function to be put in between two args
findWordInLine = isInfixOf
-- how to create a partial from isInfixOf?
-- the first form
-- ss = isInfixOf "iddqd"
-- hardcode the needle
-- the second form
-- ss a = isInfixOf a "iddqd"
-- hardcode the haystack

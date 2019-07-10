#!/usr/bin/env stack runghc

import System.IO
import Control.Monad
import Control.Applicative

-- haskell design pattern P/78
-- IO as a first class citizen
-- this code looks imperative in style

-- what is imperative style
-- https://en.wikipedia.org/wiki/Imperative_programming
-- a programming paradigm that uses statements that change a 
-- program's state. In much the same way that the imperative mood 
-- in natural languages expresses commands, an imperative program 
-- consists of commands for the computer to perform. Imperative 
-- programming focuses on describing how a program operates. 

-- what is declarative style
-- https://en.wikipedia.org/wiki/Declarative_programming
-- a programming paradigm—a style of building the structure and 
-- elements of computer programs—that expresses the logic of a 
-- computation without describing its control flow.

-- functions can return IO actions; functions can take IO actions 
-- as arguments. We can compose regular functions with functions 
-- that return IO actions -- hence the first class citizen notion

demoOpenAndReadOneLine :: IO ()
demoOpenAndReadOneLine = do
    print "//// demo open and read one line"
    h <- openFile "records.txt" ReadMode
    line <- hGetLine h
    putStrLn . show . words $ line
    hClose h

-- do is a syntactic sugar for bind 
-- what is bind
-- https://www.haskell.org/tutorial/monads.html
-- The Monad class defines two basic operators: >>= (bind) and 
-- return. The bind operations, >> and >>=, combine two monadic 
-- values while the return operation injects a value into the 
-- monad (container).
demoOpenAndReadOneLineBind = do
    print "//// demo open and read one line - bind operator"
    h <- openFile "records.txt" ReadMode
    hGetLine h >>= print . words
    hClose h

-- haskell design pattern P/79
-- IO monad is also an applicative functor, which in turn is also
-- a functor; 
demoOpenAndReadOneFmap = do
    print "//// demo open and read one line - functor"
    h <- openFile "records.txt" ReadMode
    -- IO as a functor allows us to use the fmap function in the 
    -- result of the hGetLine action
    line1 <- fmap (show . words) (hGetLine h)
    putStrLn line1
    -- IO as an applicative functor means that we can use this
    -- syntax (instead of fmap)
    line2 <- (show. words) <$> (hGetLine h)
    putStrLn line2
    -- can also use liftM function
    line3 <- liftM (show . words) (hGetLine h)
    putStrLn line3

    -- monad is more powerful than applicative and applicative
    -- more powerfun than functor
    -- monad enables us to compose IO actions together in 
    -- sequenced pipelines;
    -- functor and applicative allow us to apply functions to 
    -- IO actions
    hClose h

-- there is just one bind, using the bind notation is more concise
demoOpenError = do
    print "//// demo open and read one line - error"
    h <- openFile "notafile.txt" ReadMode
    line <- hGetLine h
    putStrLn . show . words $ line
    hClose h

main :: IO ()
main = do
    demoOpenAndReadOneLine
    demoOpenAndReadOneLineBind
    demoOpenAndReadOneFmap
    demoOpenError

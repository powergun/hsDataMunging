#!/usr/bin/env stack runghc

-- recall C++ (boost)'s lexical_cast<>
-- https://www.boost.org/doc/libs/1_42_0/libs/conversion/lexical_cast.htm
-- (lazy) bytestring offers readInt() etc.. functions to perform
-- per-type conversion
-- the builtin string uses Read typeclass

-- real world haskell P/181
-- Read typeclass is essentially the opposite of Show
-- it defines functions that will take a string, parse it, and
-- return the data in any type that is a member of Read

-- the most useful function is read()
-- P/181
-- use explicit Double type to avoid compiler guessing
-- NOTE the syntax of explicit type (I had done this in other
-- examples)
demoShowRead = do
  let inStr = "3.14167" 
      inpDouble = (read inStr)::Double
  putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

-- P/183
-- while it is possible to build sophisticated parsers using
-- the Read typeclass, many people find it easier to do so 
-- using Parsec, and rely on Read only for simple tasks.

main :: IO ()
main = do
  demoShowRead

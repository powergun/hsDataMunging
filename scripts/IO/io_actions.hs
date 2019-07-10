#!/usr/bin/env stack runghc

-- real world haskell P/206
-- anything that is IO something is an IO action
-- you can store it and nothing will happen
-- I could say writefoo = putStrLn "foo" and nothing happens
-- right then; but if I later use writefoo in the middle of 
-- another IO action, the writefoo action will be executed 
-- when its parent action is executed
-- IO actions can be glued together to form bigger IO actions.

-- the () is an empty tuple (pronounced unit), indicating that
-- there is no return value from putStrLn
-- this is similar to void in C
-- the type if value () is also ()

-- actions can be created, assigned and passed anywhere.
-- however they may only be performed (executed) from within 
-- another IO action

-- real world haskell P/208
-- when you are working in a do block, 
-- use <- to get results from IO actions and 
-- let to get results from pure code
-- when used in a do block, you should not put in after your let statement
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- real world haskell P/225
-- you could think of it this way: every statement (in do block)
-- except let,  must yield an IO action that will be executed

str2message :: String -> String 
str2message input = "Data: " ++ input

str2action :: String -> IO () 
-- convert a string to an IO action
-- separate IO actions generated here are to be combined to a 
-- big action, to be executed
str2action = putStrLn . str2message

numbers :: [Int] 
numbers = [1..10]

main = do 
  str2action "Start of the program" 
  -- P/225
  -- this function is similar to map.
  -- it takes a function and a list.
  -- the function supplied to mapM_ is an IO action that is executed 
  -- for every item in the list
  -- mapM_ throws out the result of the function, through you 
  -- can use mapM to return a list of IO results if you want them

  -- functions that end with an underscore typically discard 
  -- their result
  mapM_ (str2action . show) numbers

  -- result is [(), ()....] because str2action returns ()
  result <- mapM (str2action . show) numbers
  print result
  str2action "Done!"

-- map is a pure function that returns a list
-- it doesn't and can't actually execute actions directly
-- mapM is a utility that lives in the IO monad and thus can 
-- actually execute the actions (it combines a bunch of separate
-- IO actions into a big action; the separate actions are executed 
-- when the big action is)





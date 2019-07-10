#!/usr/bin/env stack runghc

-- real world haskell P/227
-- >>
-- sequence two actions; the result of the computation is the 
-- result of the second action (first's is thrown away)
-- >>=
-- runs an action (first), passes its result to a function 
-- that returns an action; the second is run as well, and the 
-- result of the entire expression is the result of that second
-- action
main =
  putStrLn "Greetings! What is your name?" >>
  getLine >>= (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")


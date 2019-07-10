#!/usr/bin/env stack runghc

-- real world haskell P/228
-- use <- in combination with return, but let in combination
-- with simple literal; that's because we needed both values
-- to be pure in order to add them, and <- pulls things out 
--                                      ^^^^^^^^^^^^^^^^^^^
-- of monads, effectively reversing the effect of return
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
main :: IO () 
main = do 
  one <- return 1 
  let two = 2
  putStrLn $ show (one + two)

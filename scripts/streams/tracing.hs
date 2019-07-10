#!/usr/bin/env stack runghc

-- import & operator
import Data.Function
import Debug.Trace

-- demo a technique shown in udemy mastering haskell programming
-- to trace a stream pipeline

source :: [Int]
source = map (\x -> trace ("source " ++ show x) x) [1..3]

odds :: [Int] -> [Int]
odds = filter (\x -> trace ("filter " ++ show x) $ odd x)

sink :: [Int] -> Int
sink [] = 0
sink (x:xs) = trace ("sink " ++ show x) (x + sink xs)

demoStreamTracing :: IO ()
demoStreamTracing = do
  -- WTF is &
  -- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Function.html

  -- (&) :: a -> (a -> b) -> b infixl 1
  -- & is a reverse application operator. This provides notational 
  -- convenience. Its precedence is one higher than that of the 
  -- forward application operator $, which allows & to be nested 
  -- in $.
  -- >>> 5 & (+1) & show
  -- "6"

  let r = source & odds & sink
  -- equivalent to 
  -- let r = sink $ odds $ source
  print r

main :: IO ()
main = do
  demoStreamTracing

#!/usr/bin/env stack runghc

-- when source, transform and sink invoke IO (have side effect)
-- NOTE this version does not utilize stream processing 
-- all the data has to be fully processed in each step function 
-- before the next function consume it
-- this can be seen from the printed messages
-- "source 1"
-- "source 2"
-- "source 3"
-- "filter 1"
-- "filter 2"
-- "filter 3"
-- "sink 1"
-- "sink 3"

source :: IO [Int]
source = go 1
  where
    go 4 = return []
    go x = do
      print $ "source " ++ show x
      xs <- go (x + 1)
      return (x:xs)

odds :: [Int] -> IO [Int]
odds [] = return []
odds (x:xs) = do
  print $ "filter " ++ show x
  xs' <- odds xs
  return $ if odd x then x:xs' else xs'

sink :: [Int] -> IO Int
sink [] = return 0
sink (x:xs) = do
  print $ "sink " ++ show x
  (x +) <$> sink xs

demoStreamPipeline :: IO ()
demoStreamPipeline = do
  r <- source >>= odds >>= sink
  print r

main :: IO ()
main = do
  demoStreamPipeline

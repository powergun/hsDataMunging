#!/usr/bin/env stack runghc

-- this version use unsafeInterleaveIO Prelude function to
-- achieve true streaming processing
-- unsafeInterleaveIO delays these calls until they are needed

import System.IO.Unsafe

source :: IO [Int]
source = go 1
  where
    go 4 = return []
    go x = do
      print $ "source " ++ show x
      xs <- unsafeInterleaveIO $ go (x + 1)
      return (x:xs)

odds :: [Int] -> IO [Int]
odds [] = return []
odds (x:xs) = do
  print $ "filter " ++ show x
  xs' <- unsafeInterleaveIO $ odds xs
  return $ if odd x then x:xs' else xs'

sink :: [Int] -> IO Int
sink [] = return 0
sink (x:xs) = do
  print $ "sink " ++ show x
  (x +) <$> unsafeInterleaveIO (sink xs)

demoStreamPipeline :: IO ()
demoStreamPipeline = do
  r <- source >>= odds >>= sink
  print r

main :: IO ()
main = do
  demoStreamPipeline
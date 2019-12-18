module Streamly_.ConcurStreamGen (demo) where

-- source:
-- https://hackage.haskell.org/package/streamly-0.6.1

import           Streamly
import           Streamly.Prelude   (nil, (|:))
import qualified Streamly.Prelude   as S

import           Control.Concurrent
import           Control.Monad      (forever)

-- threadDelay n: n microseconds
p n = threadDelay (n * 1000000) >> return n

frontend :: Int -> IO String
frontend n = threadDelay (n * 1000) >> return ("frontend-" ++ show n)

backend :: Int -> IO String
backend n = threadDelay (n * 1000) >> return ("backend-" ++ show n)

demo :: IO ()
demo = do
  -- a <- S.toList $ getLine |: getLine |: S.nil
  -- a <- S.toList $ frontend |: backend |: S.nil

  -- generate parallel stream

  -- print =<< (S.toList $ parallely $ frontend 3 |: backend 1 |: S.nil)
  -- print =<< (S.toList $ frontend 3 |: backend 1 |: S.nil)

  -- async effect (parallelism) is more noticeable when horizontally scaled
  -- parallel
  runStream $ asyncly $ S.replicateM 10 $ frontend 1000
  -- serial
  -- runStream $ S.replicateM 10 $ frontend 1000

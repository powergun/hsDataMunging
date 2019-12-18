module Streamly_.GenerateStreams (demo) where

-- source:
-- https://hackage.haskell.org/package/streamly-0.6.1

import           Streamly
import           Streamly.Prelude   (nil, (|:))
import qualified Streamly.Prelude   as S

import           Control.Concurrent
import           Control.Monad      (forever)

frontend :: IO String
frontend = return "asd"

backend :: IO String
backend = return "[...]"

demo :: IO ()
demo = do
  -- generate serial stream

  -- a <- S.toList $ getLine |: getLine |: S.nil
  -- a <- S.toList $ frontend |: backend |: S.nil

  -- generate parallel stream
  a <- S.toList $ parallely $ frontend |: backend |: S.nil
  print a


module Streamly_.GenerateStreams (demo) where

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
  -- a <- S.toList $ getLine |: getLine |: S.nil
  a <- S.toList $ frontend |: backend |: S.nil
  print a


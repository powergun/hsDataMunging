module Streamly_.Serial (demo) where

import           Streamly
import           Streamly.Prelude   (nil, (|:))
import qualified Streamly.Prelude   as S

import           Control.Concurrent
import           Control.Monad      (forever)

demo :: IO ()
demo = do
  return ()

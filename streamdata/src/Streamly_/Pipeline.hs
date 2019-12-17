module Streamly_.Pipeline (demo) where

import           Data.Function    ((&))
import           Streamly
import qualified Streamly.Prelude as S

demo = runStream $
        S.repeatM getLine
      & fmap read
      & S.filter even
      & S.takeWhile (<= 9)
      & fmap (\x -> x * x)
      & S.mapM print

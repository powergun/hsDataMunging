{-# LANGUAGE OverloadedStrings #-}
module Exceptions.EitherParse (demo) where

import           Data.Aeson
import           Data.Text

demo :: IO ()
demo = do
  let s = "[1, 2, 3, 4, true]"
      -- Left "Error in $[4]: expected Int, encountered Boolean"
      d = (eitherDecode s) :: Either String [Int]
  print d

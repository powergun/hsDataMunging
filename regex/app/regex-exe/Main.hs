module Main where

import qualified DemoPosix
import qualified DemoPcre
import qualified GlobRegex
import qualified Glob
import qualified DemoGrep
main :: IO ()
main = do
  DemoPosix.demo
  DemoPcre.demo
  GlobRegex.demo
  Glob.demo
  DemoGrep.demo
  
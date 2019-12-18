module Streamly_.RecurReadFiles (demo) where

import           Streamly
import           Streamly.Prelude   (nil, (|:))
import qualified Streamly.Prelude   as S
-- Data.Text.IO.readFile.
import           Control.Concurrent
import           Control.Exception
import           Control.Monad      (forever)
import           Data.Either
import qualified Data.Text
import qualified Data.Text.IO

safeReadFile :: FilePath -> IO (Either IOException Data.Text.Text)
safeReadFile = try . Data.Text.IO.readFile

doReadFile :: FilePath -> IO Int
doReadFile filename = do
  ret <- safeReadFile filename
  either (\_ -> return 0) (return . length . Data.Text.lines) ret

demo :: IO ()
demo = do
  filenames <- return . lines =<< getContents
  -- print filenames

  -- runStream $ asyncly $ S.replicateM 10 $ frontend 1000

  -- blinded calling paralleled IO operation will result in
  -- recur: app/cbe/keypairs/services/app/prod.tfstate: openFile: resource exhausted (Too many open files)
  -- naive bucketize won't fix it!!!
  -- answer is here:
  -- https://stackoverflow.com/questions/22893168/preventing-getcurrentdirectory-resource-exhausted-too-many-open-files-error
  -- I must use a strict readFile
  -- I took a step further to handle these invalid files (using either)
  {-
  naive
  real	0m36.817s
  user	0m42.605s
  sys	0m5.592s

  streamly
  real	0m5.234s
  user	0m9.180s
  sys	0m2.981s
  -}
  s <- S.toList $ asyncly $ foldMap (S.yieldM . doReadFile) filenames
  print . length $ s

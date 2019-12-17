module Streamly_.RecurDirectory (demo) where

import           Control.Monad.IO.Class (liftIO)
import           Path                   (Path)
import           Path.IO                (getCurrentDir, listDir)

import           Streamly               (aheadly, runStream)


listDirRecursive = getCurrentDir >>= readdir
  where
    readdir dir = do
      (dirs, files) <- listDir dir
      liftIO $ mapM_ putStrLn
             $ map show dirs ++ map show files
      foldMap readdir dirs


{-
comparing the serial and parallel version (using CA's infra repo)

real	0m25.116s
user	0m12.797s
sys	0m10.210s

real	0m13.468s
user	0m20.392s
sys	0m12.292s
-}
demo :: IO ()
demo =
  -- listDirRecursive
  runStream $ aheadly $ listDirRecursive

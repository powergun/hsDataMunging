module IOStreams.MakeStream where

import           Data.IORef                     ( newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           System.Random                  ( randomIO )
import           System.IO.Streams              ( Generator
                                                , InputStream
                                                , OutputStream
                                                )
import qualified System.IO.Streams             as S

randomInputStreamGen :: Int -> IO (InputStream Double)
randomInputStreamGen count = S.fromGenerator (go count)
 where
  go :: Int -> Generator Double ()
  go 0 = return ()
  go n = liftIO randomIO >>= S.yield >> go (n - 1)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Herestring.StaticByteStr (demo) where

import qualified Data.ByteString.Char8 as BS
import           Text.RawString.QQ

document :: BS.ByteString
document = [r|there is a silence,
  where hath been no sound.
  there is a silence,
  where no sounds hath been.
|]

demo :: IO ()
demo = do
  print . BS.lines $ document

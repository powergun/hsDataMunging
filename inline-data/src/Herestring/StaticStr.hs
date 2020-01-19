{-# LANGUAGE QuasiQuotes #-}
module Herestring.StaticStr (demo) where

import           Text.RawString.QQ

document :: String
document = [r|there is a silence,
where hath been no sound.
there is a silence,
where no sounds hath been.
|]

demo :: IO ()
demo = do
  print . lines $ document

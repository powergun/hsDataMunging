{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Herestring.StaticText (demo) where

import qualified Data.Text         as T
import           Text.RawString.QQ

document :: T.Text
document = [r|there is a silence,
  where hath been no sound.
  there is a silence,
  where no sounds hath been.
|]

demo :: IO ()
demo = do
  print . T.lines $ document

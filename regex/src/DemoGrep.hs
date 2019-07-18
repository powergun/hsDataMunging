module DemoGrep
  ( demo
  ) where

import Text.Regex.PCRE ((=~))

demo :: IO ()
demo = do
  print
    "//////// demo grep-like regex application ////////////////"
  let txt = ["there", "is", " a", "c ow"]
  print $ filter (\elem -> elem =~ "\\s+" :: Bool) txt

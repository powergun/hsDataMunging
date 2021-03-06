{-# LANGUAGE OverloadedStrings #-}

module SimpleDemo (demo) where

import qualified Data.Yaml as Y

demo :: IO ()
demo = do
  res <- Y.decodeThrow "[1,2,3]"
  print (res :: [Integer])

-- You can go one step further and decode into a custom type by implementing
-- 'FromJSON' for that type. This is also appropriate where extra
-- normalization, formatting or manipulation of the YAML is required on decode.

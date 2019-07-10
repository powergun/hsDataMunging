#!/usr/bin/env stack runghc

-- motivation:
-- intrigued by real world haskell P/113

matchFirstTwo :: String -> Bool
matchFirstTwo cs =
  case cs of
    -- NOTE I can NOT use variable in the place of 't', 'h' as 
    -- they will become wildcard
    ('t':'h':rest) -> True
    _ -> False

main :: IO ()
main = do
  print $ matchFirstTwo "thereisacow"
  print $ matchFirstTwo "settherei"

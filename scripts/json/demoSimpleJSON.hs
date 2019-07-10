#!/usr/bin/env stack runghc

import SimpleJSON
import PrintJSON

createDemoJObject =
  JObject [
    ("map", JObject [
      ("episode", JString "e1"),
      ("code", JString "m1")]),
    ("creatures", JArray [
      JString "imp", JString "gunner"
      ])
  ]

demoCreateJValue :: IO ()
demoCreateJValue = do
  print "//// demo create JValue"
  print $ JArray [JString "asd", JNull, createDemoJObject]
  print $ [asString $ JString "asd", asString $ JNumber 1]

-- to test, pipe the result to jq
-- real world haskell P/156
-- best practice: separate IO code (code that does printing) 
-- from pure code
-- the idea of separating pure code from impure code is powerful,
-- and it is pervasive in haskell code
-- (example: compression library function uses String -> String
-- function interface)
demoRenderJValue :: IO ()
demoRenderJValue = do
  print "//// demo renderJValue"
  putStrLn $ renderJValue $ createDemoJObject

main :: IO ()
main = do
  demoCreateJValue
  demoRenderJValue

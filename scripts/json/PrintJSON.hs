module PrintJSON 
 ( renderJValue

 ) where

import Data.List (intercalate)

import SimpleJSON

-- real world haskell P/159
-- developing haskell module without going nuts
-- one useful technique for quickly developing the skeleton of
-- a program is to write placeholder, or stub, versions of types 
-- and functions.
-- data Doc = ToBeDefined
--   deriving (Show)
-- string :: String -> Doc
-- string str = undefined
-- text :: String -> Doc
-- text str = undefined
-- double :: Double -> Doc
-- double num = undefined

renderJValue :: JValue -> String

renderJValue JNull = "null"

renderJValue (JNumber n) = show n

renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"

renderJValue (JString s) = show s

renderJValue (JObject o) =
  "{ " ++ renderPairs o ++ " }" 
  where
    renderPairs [] = ""
    renderPairs ps = intercalate ", " (map renderPair ps)
    -- JSON key must be double-quoted (hence calling show k)
    renderPair (k, v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = 
  "[ " ++ renderValues a ++ " ]"
  where
    renderValues [] = ""
    renderValues vs = intercalate ", " (map renderJValue vs)

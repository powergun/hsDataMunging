module SimpleJSON
  (
    JValue(..)
  , asArray
  , asBool
  , asDouble
  , asInt
  , asObject
  , asString
  ) where

-- real world haskell P/151
-- to work with JSON data in haskell, we use an algebraic 
-- data type to represent the range of possible JSON types
-- NOTE: this pattern is also found in shellcheck

-- JObject: recursive type, take a list of key-value renderPairs
-- NOTE JObject value ctor has type (String, JValue) (not JString)

-- TODO: real world haskell P/190
-- TODO: use typeclass to define flexible factory interface
-- toJValue: work with any types; caller does not need to be
--           aware of the implementation
-- fromJValue: ditto
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Show, Eq, Ord)

asString :: JValue -> Maybe String
asString (JString s) = Just s
asString _ = Nothing
asInt (JNumber n) = Just (truncate n)
asInt _ = Nothing
asDouble (JNumber n) = Just n
asDouble _ = Nothing
asBool (JBool b) = Just b
asBool _ = Nothing
asObject (JObject o) = Just o
asObject _ = Nothing
asArray (JArray a) = Just a
asArray _ = Nothing

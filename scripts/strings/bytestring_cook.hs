#!/usr/bin/env stack runghc

{-# LANGUAGE OverloadedStrings #-}
-- haskell cookbook L4022
-- this extension allows us to use string of types string, text
-- and bytestring

-- all these data types are instances of IsString
-- if a data type is an instance of IsString then the extension
-- OverloadedStrings can be applied in the context of that data
-- type

-- one of the advantages of using the overloaded extension is that 
-- we can use a quoted string without having to explicitly 
-- convert it from the built-in string data type

-- L4015
-- while text implements unicode characters, bytestring is good 
-- for binary data

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import System.IO

-- L4039
demoByteStringCreation :: IO ()
demoByteStringCreation = do
  let bString = "there is a cow" :: B.ByteString
      uniString = "三国志" :: B.ByteString
  print bString
  print uniString -- "\t\253\215", completely losing the identity

demoTextCreation :: IO ()
demoTextCreation = do
  let tString = "there is a cow" :: T.Text
      uniString = "三国志" :: T.Text
  print tString
  print uniString -- "\19977\22269\24535", preserve the identity

main :: IO ()
main = do
  demoByteStringCreation
  demoTextCreation

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
  print uniString -- "\t\253\215", completely losing the codepoint

demoTextCreation :: IO ()
demoTextCreation = do
  let tString = "there is a cow" :: T.Text
      uniString = "三国志" :: T.Text
  print tString
  print uniString -- "\19977\22269\24535", preserve the codepoint

demoStringToByteString :: IO ()
demoStringToByteString = do
  let bString = B.pack "there is a cow"
  print bString

demoStringToText :: IO ()
demoStringToText = do
  let tString = T.pack "三国志"
  print tString
  print $ T.pack "there is a cow"

demoByteStringToString :: IO ()
demoByteStringToString = do
  let s = B.unpack "there is a cow"
  print s

demoTextToString :: IO ()
demoTextToString = do
  let s = T.unpack "there is a cow"
  print s

demoByteStringToText :: IO ()
demoByteStringToText = do
  -- text to bytestring (unicode to bytes)
  let bytes = TE.encodeUtf8 "三国志"
      uniStr = TE.decodeUtf8 bytes
  print bytes -- "\228\184\137\229\155\189\229\191\151"
  print uniStr -- "\19977\22269\24535"

demoListFunctionsOnText :: IO ()
demoListFunctionsOnText = do
  print $ (T.head "First", T.tail "First")

demoListFunctionsOnByteString :: IO ()
demoListFunctionsOnByteString = do
  print $ (B.head "First", B.tail "First")

demoPrint :: IO ()
demoPrint = do
  -- L4074
  -- text, bytestring modules define their own versions of the 
  -- system.IO functions
  -- MY NOTES:
  -- putStrLn is able to print the unicode character correctly
  -- print() can not
  TIO.putStrLn ("三国志|there is a cow" :: T.Text)
  B.putStrLn ("三国志|there is a cow" :: B.ByteString)
  withFile "/var/tmp/sut/tt.txt" WriteMode $ \h -> do
    TIO.hPutStrLn h "title:三国志"
    B.hPutStrLn h "title:三国志"
  {-
  title:三国志
  title:  ��
  -}

main :: IO ()
main = do
  demoByteStringCreation
  demoTextCreation
  demoStringToByteString
  demoStringToText
  demoByteStringToString
  demoTextToString

  demoByteStringToText

  demoListFunctionsOnText
  demoListFunctionsOnByteString

  demoPrint
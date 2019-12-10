{-# LANGUAGE OverloadedStrings #-}

module AnyTypes.StringVisitor (demo) where

import           Data.Aeson

import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           GHC.Exts

import qualified Data.ByteString.Lazy as BL
import           Data.Maybe

revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array x)  = Array (fmap revStrings x)
revStrings (Object x) = let revPair (k, v) = (T.reverse k, revStrings v)
                        in  Object . fromList . map revPair . HM.toList $ x
revStrings other      = other

revJSON = encode . revStrings . fromJust . decode

demo :: IO ()
demo = do
  s <- BL.readFile "./testdata/anytypes.json"
  print . revJSON $ s

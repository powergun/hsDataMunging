{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AnyTypes.StringVisitor
  ( demo
  )
where

import           Data.Aeson

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           GHC.Exts

import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe
import           Text.RawString.QQ

revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array  x) = Array (fmap revStrings x)
revStrings (Object x) =
  let revPair (k, v) = (T.reverse k, revStrings v)
  in  Object . fromList . map revPair . HM.toList $ x
revStrings other = other

revJSON = encode . revStrings . fromJust . decode

sut :: BL.ByteString
sut = [r|
{
  "folders": [
    {
      "path": "javascript/github.com/Wei-N-Ning/jsExamples/serverless/serverless-big-mouths"
    },
    {
      "doom": "1993"
    }
  ],
  "settings": {
    "editor.tabSize": 2
  }
}
|]

demo :: IO ()
demo = do
  -- sut <- BL.readFile "./testdata/anytypes.json"
  putStrLn "//// string visitor example: reverse string fields"
  print . revJSON $ sut

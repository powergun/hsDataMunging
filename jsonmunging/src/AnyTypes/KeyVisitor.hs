{-# LANGUAGE OverloadedStrings #-}
module AnyTypes.KeyVisitor
  ( demo
  )
where

import           Data.Aeson

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           GHC.Exts

import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe

visit :: (T.Text -> Bool) -> Value -> IO ()
visit f (Object o) = mapM_ visitPair (HM.toList o)
 where
  visitPair (k, v) = case (f k) of
    True  -> print v
    False -> visit f v
visit f (Array x) = mapM_ (visit f) x
visit f other     = return ()

edit :: (T.Text -> Bool) -> (Value -> Value) -> Value -> Value
edit f g (Object o) = Object . fromList . map editPair . HM.toList $ o
 where
  editPair (k, v) = case (f k) of
    True  -> (k, g v)
    False -> (k, edit f g v)
edit f g (Array ar) = Array $ fmap (edit f g) ar
edit f g other      = other

editValue :: Value -> Value
editValue (String s) = String . T.reverse $ s
editValue other      = other

demo :: IO ()
demo = do
  print "<key-visitor>"
  s <- BL.readFile "./testdata/anytypes.json"
  (visit (== "path")) . fromJust . decode $ s
  (visit (== "doom")) . fromJust . decode $ s
  print "</key-visitor>"

  print "<key-edit>"
  let s' = edit (== "path") editValue (fromJust . decode $ s)
  print . encode $ s'
  let s'' = edit (== "doom") editValue (fromJust . decode $ s)
  print . encode $ s''
  print "</key-edit>"


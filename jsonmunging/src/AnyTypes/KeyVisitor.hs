{-# LANGUAGE OverloadedStrings #-}
module AnyTypes.KeyVisitor (demo) where

import           Data.Aeson

import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           GHC.Exts

import qualified Data.ByteString.Lazy as BL
import           Data.Maybe

visit :: (T.Text -> Bool) -> Value -> IO ()
visit f (Object o) =
    mapM_ visitPair (HM.toList o)
  where
    visitPair (k, v) = case (f k) of
                         True  -> print v
                         False -> visit f v
visit f (Array x) = mapM_ (visit f) x
visit f other = return ()

demo :: IO ()
demo = do
  print "<key-visitor>"
  s <- BL.readFile "./testdata/anytypes.json"
  (visit (== "path")) . fromJust . decode $ s
  (visit (== "doom")) . fromJust . decode $ s
  print "</key-visitor>"

{-# LANGUAGE OverloadedStrings #-}

module AnyTypes.JSONTypes (demo) where

import           Data.Aeson

val :: Value
val = object [
  "boolean" .= True,
  "numbers" .= [1,2,3::Int] ]    -- a type annotation is needed because
                                  -- otherwise it's unclear whether it should
                                  -- be Int or, say, Double or Rational

demo :: IO ()
demo = do
  print $ encode val

module Main where

import qualified Control.Monad.State as Ms
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

-- source
-- sed implementation in haskell video

data SedState = SedState {
  line         :: Int
, input        :: T.Text
, patternSpace :: T.Text
, holdSpace    :: T.Text
, output       :: T.Text
} deriving (Show)

data Command = Print
             | Next
             | Delete
             | Substitute String String String
             deriving (Show)

-- reason for using Text type: Text allows utf-8 encoding, whereas
-- String is ascii
-- wrapping the monadial function! use evalState to pull the result
sed :: [Command] -> T.Text -> T.Text
sed cmds t =
  -- note: I can not use t as the default SedState! it must be
  -- constructed
  Ms.evalState (runCommands cmds) (defaultState t)

defaultState :: T.Text -> SedState
defaultState t =
  SedState 1 t (head (T.lines t)) T.empty T.empty

runCommands :: [Command] -> Ms.State SedState T.Text
-- because this function needs to "return T.Text" instead of
-- Ms.State (the type returned by "calling return()"), I need
-- an accessor here, and compose it with return
runCommands cmds =
  Ms.get >>= return <$> output

main :: IO ()
-- Prelude interact accepts String -> String function, therefore
-- I need to use the TIO version of interact
main = TIO.interact $ sed [Print, Next, Delete]

module Main where

import qualified Control.Monad       as M
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
  -- constructed; see the definition of defaultState function
  Ms.evalState (runCommands cmds) (defaultState t)

defaultState :: T.Text -> SedState
defaultState t =
  SedState 1 t (head (T.lines t)) T.empty T.empty

runCommands :: [Command] -> Ms.State SedState T.Text
-- because this function needs to "return T.Text" instead of
-- Ms.State (the type returned by "calling return()"), I need
-- an accessor here, and compose it with return
runCommands cmds = do
  M.mapM_ runCommand cmds
  ss <- Ms.get
  if line ss == length (T.lines $ input ss)
  then return (output ss)
  else runCommand Next >> runCommands cmds

-- note that the video uses the bind command >>= so it has
-- to define a new lambda function for each new action
-- \\\\ this is better \\\\
-- ss <- Ms.get
-- ss { output = output ss `T.append` patternSpace ss }
-- \\\\ this is even better \\\\
runCommand :: Command -> Ms.State SedState ()
runCommand Print =
  Ms.modify $ \ss -> ss { output = output ss `T.append` patternSpace ss }

runCommand Next =
  Ms.modify $ \ss -> ss {
    line = line ss + 1
  , patternSpace = (T.lines (input ss)) !! line ss
  }

main :: IO ()
-- Prelude interact accepts String -> String function, therefore
-- I need to use the TIO version of interact
main = TIO.interact $ sed [Print]

module Main where

import qualified Control.Monad       as M
import qualified Control.Monad.State as Ms
-- what is zipper: https://wiki.haskell.org/Zipper
import qualified Data.List.Zipper    as Z
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

-- source
-- sed implementation in haskell video

data SedState = SedState {
  line         :: Int
, input        :: Z.Zipper T.Text
, patternSpace :: T.Text
, holdSpace    :: T.Text
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
  SedState 1 (Z.delete z) (Z.cursor z) (T.singleton '\n')
  where
    z = Z.fromList (T.lines t)

runCommands :: [Command] -> Ms.State SedState T.Text
-- because this function needs to "return T.Text" instead of
-- Ms.State (the type returned by "calling return()"), I need
-- an accessor here, and compose it with return
runCommands cmds = do
  M.mapM_ runCommand cmds
  ss <- Ms.get
  if Z.endp $ input ss
  then (return . T.unlines . Z.toList) (input ss)
  else runCommand Next >> runCommands cmds

-- note that the video uses the bind command >>= so it has
-- to define a new lambda function for each new action
-- \\\\ this is better \\\\
-- ss <- Ms.get
-- ss { output = output ss `T.append` patternSpace ss }
-- \\\\ this is even better \\\\
runCommand :: Command -> Ms.State SedState ()
runCommand Print =
  Ms.modify $ \ss -> ss { input = Z.push (patternSpace ss) (input ss) }
  -- ss is not seen in "where" block; I have to use let..in

runCommand Next =
  Ms.modify $ \ss -> ss {
    line = line ss + 1
  , input = Z.delete (input ss)
  , patternSpace = Z.cursor (input ss)
  }

runCommand Delete =
  Ms.modify $ \ss -> ss { patternSpace = T.empty }

runCommand (Substitute pat sub flags) =
  Ms.modify id

-- the naive "unlines . lines" concatenation logic for T.Text
-- is O(n^2); therefore I need to introduce a O(n) concat
-- it also needs to account for the newline character
(<+>) :: T.Text -> T.Text -> T.Text
(<+>) t1 t2 = t1 `T.append` T.cons '\n' t2

main :: IO ()
-- Prelude interact accepts String -> String function, therefore
-- I need to use the TIO version of interact
main = TIO.interact $ sed [Print]

module SedLib (sed) where

import qualified Control.Monad       as M
import qualified Control.Monad.State as Ms
-- what is zipper: https://wiki.haskell.org/Zipper
import           Command
import qualified Data.List.Zipper    as Z
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Debug.Trace
import           Parser
import qualified Text.Regex          as Regex

-- source
-- sed implementation in haskell video

data SedState = SedState {
  line         :: Int
, zipper       :: Z.Zipper T.Text
, patternSpace :: T.Text
, holdSpace    :: T.Text
} deriving (Show)

-- reason for using Text type: Text allows utf-8 encoding, whereas
-- String is ascii
-- wrapping the monadial function! use evalState to pull the result
sed :: String -> Bool -> T.Text -> T.Text
sed s n t =
  let cmds = parseSed s
  -- note: I can not use t as the default SedState! it must be
  -- constructed; see the definition of defaultState function
  in Ms.evalState (runCommands cmds n) (defaultState t)

defaultState :: T.Text -> SedState
defaultState t =
  SedState 1 (Z.delete z) (Z.cursor z) (T.singleton '\n')
  where
    z = Z.fromList (T.lines t)

runCommands :: [Command] -> Bool -> Ms.State SedState T.Text
-- because this function needs to "return T.Text" instead of
-- Ms.State (the type returned by "calling return()"), I need
-- an accessor here, and compose it with return
runCommands cmds quiet = do
  M.mapM_ runCommand cmds
  M.unless quiet (runCommand Print)
  ss <- Ms.get
  if Z.endp $ zipper ss
  then (return . T.unlines . Z.toList) (zipper ss)
  else runCommand Next >> runCommands cmds quiet

-- note that the video uses the bind command >>= so it has
-- to define a new lambda function for each new action
-- \\\\ this is better \\\\
-- ss <- Ms.get
-- ss { output = output ss `T.append` patternSpace ss }
-- \\\\ this is even better \\\\
runCommand :: Command -> Ms.State SedState ()
runCommand Print =
  Ms.modify $ \ss -> ss { zipper = Z.push (patternSpace ss) (zipper ss) }
  -- ss is not seen in "where" block; I have to use let..in

runCommand Next = Ms.modify $ \ss ->
  ss { line = line ss + 1
     , zipper = Z.delete (zipper ss)
     , patternSpace = if Z.endp (zipper ss)
                      then T.empty
                      else Z.cursor (zipper ss)
     }

runCommand Delete = Ms.modify $ \ss -> ss { patternSpace = T.empty }

runCommand (Substitute pat sub flags) = Ms.modify $ \ss ->
  -- replacement string takes \\0, \\1, \\2 that refer to the capture
  -- perl deals with this in a similar way
  -- perl -n -E 's/(\d+).(\d+)/\1--\2/g and say' <<<"234.123 3241.213123"
  -- 234--123 3241--213123
  let newText = T.pack (Regex.subRegex regex src sub)
      regex = Regex.mkRegex pat
      src = T.unpack (patternSpace ss)
  in ss { patternSpace = newText }

-- the naive "unlines . lines" concatenation logic for T.Text
-- is O(n^2); therefore I need to introduce a O(n) concat
-- it also needs to account for the newline character
(<+>) :: T.Text -> T.Text -> T.Text
(<+>) t1 t2 = t1 `T.append` T.cons '\n' t2

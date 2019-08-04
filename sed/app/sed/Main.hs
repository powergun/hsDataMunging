module Main where

import           Control.Monad         (when)
import           Data.List
import qualified Data.Text.IO          as TIO
import           SedLib
import           System.Console.GetOpt
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure, exitSuccess)
import           System.IO
data Flag = Help
          | Quiet
          | Expression String
          | Script String
          deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option "h" ["help"]
      (NoArg Help)
      "display this message"
  , Option "n" ["quiet"]
      (NoArg Quiet)
      "suppresses automatic printing"
  , Option "e" ["expression"]
      (ReqArg Expression "EXPRESSION")
      "commands to execute"
  , Option "f" ["file"]
      (ReqArg Script "FILE")
      "script file to execute"
  ]

helpMessage :: String
helpMessage = usageInfo title options
  where
    title = "========\n" ++
            "= hsed =\n" ++
            "========\n\n" ++
            "Usage:"

getExpression :: Flag -> IO String
getExpression (Expression s) = return s
getExpression (Script f)     = readFile f
getExpression _              = return ""

main :: IO ()
-- Prelude interact accepts String -> String function, therefore
-- I need to use the TIO version of interact
-- test case:
-- Substitute "[0-9]+" "classified(\\0)"
-- py -c "[print('there is %d some' % id(n)) for n in range(10)]" | hsed
main = do
  rawArgs <- getArgs
  when (null rawArgs) $ do
    putStrLn helpMessage
    exitFailure
  (flags, args, msgs) <- return (getOpt RequireOrder options rawArgs)
  when (Help `elem` flags) $ do
    putStrLn helpMessage
    exitSuccess
  rawExp <- mapM getExpression flags
  let quiet = Quiet `elem` flags
  let exp = filter (/= "") rawExp
      expStr = if null exp
               then head args
               else intercalate ";" exp
  TIO.interact $ sed expStr quiet


import qualified Data.Text                      as T
import           SedLib

import           System.Process
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     ((@?=))

sedCase :: String -> String -> IO Test
sedCase txt cs = do
  gnuSed <- readProcess "/usr/bin/sed" ["-n", cs] txt
  let ourSed = sed cs True (T.pack txt)
      t = testCase (show cs) (ourSed @?= T.pack gnuSed)
  return t

main :: IO ()
main = do
  testPrintOnlyQuiet <- sedCase "there\nis a\ncow" "p"
  defaultMain [ testPrintOnlyQuiet
              ]

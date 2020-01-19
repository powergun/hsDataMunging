
module WriteFileProblem (doWrite) where

-- can not reproduce the problem on Mac OS
doWrite = (writeFile "/var/tmp/test.html" "שלום!") :: IO ()

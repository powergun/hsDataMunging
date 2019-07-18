#!/usr/bin/env stack runghc

-- real world haskell P/221
-- there is a situation that is even more common: 
-- reading from stdin, making a conversion and writing the result 
-- to stdout

-- it takes on argument: a function of type String -> String
-- this function is passed the result of getContents() - that is 
-- standard input read lazily; the result of that function is 
-- sent to standard out

{-
generate test data
perl -E "foreach(1..4096) {print 'a'}" > /var/tmp/sut/4kb_a.txt

to test (will dump to stdout 4k characters)
./interact.hs </var/tmp/sut/4kb_a.txt
-}

import Data.Char (toUpper)

io :: (String -> String) -> IO ()
io f = interact f

main = do
  io (map toUpper)

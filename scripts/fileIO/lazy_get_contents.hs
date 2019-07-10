#!/usr/bin/env stack runghc

-- real world haskell P/218
-- hGetContents, getContents (work with stdin)
-- both return IO String; the string represents all of the data
-- at the current file pointer to the end of the file

import System.IO

{-
memory performance comparison:
!! SEE THE BOTTOM NOTES

1. generate the test data:
dd if=/dev/urandom of=/var/tmp/sut/indata bs=1024m count=1
also observe the memory usage 

2. run this haskell program; note that memory usage does not 
increase too much

3. compare the performance pattern with this python code
== FAST == no obvious mem consumption
with open('/var/tmp/sut/indata') as fp:
  contents = fp.read()
with open('/var/tmp/sut/outdata', 'w') as fp:
  fp.write(contents)

== SLOW == observe the mem consumption
with open('/var/tmp/sut/indata') as fp:
  contents = fp.read()
with open('/var/tmp/sut/outdata', 'w') as fp:
  fp.write(''.join([c for c in contents]))
-}

main :: IO ()
main = do
  infile <- openBinaryFile "/var/tmp/sut/indata" ReadMode 
  outfile <- openBinaryFile "/var/tmp/sut/outdata" WriteMode 
  inpStr <- hGetContents infile
  hPutStr outfile (map copyChar inpStr) 
  hClose infile
  hClose outfile
  where
    copyChar c = c

-- read world haskell P/220
-- haskell programmers use hGetContents as a filter quite 
-- often. They read from one file, do something to the data, and 
-- write the result out elsewhere

-- readFile uses hGetContents internally, and the underlying
-- handle will be closed when the returned String is garbage-
-- collected or all the input has been consumed

-- P/221
-- lazy output:
-- putStr (and all the similar output functions) write out data
-- as it becomes available.
-- they also have no need for keeping around data already written
-- so as long as nothing else in the program needs it, the memory 
-- can be freed immediately
-- in a sense, you can think of the String between readFile and 
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- writeFile as a pipe linking the two.
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- data goes in one end, is transformed some way, and flows 
-- back out the other

-- you should see a constant - and low - memory usage while it 
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- is being processed
-- ^^^^^^^^^^^^^^^^^

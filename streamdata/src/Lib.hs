module Lib
    ( demo
    ) where

import           Conduit
import qualified Data.Conduit.Combinators as CB
import qualified Data.Conduit.List        as CL

demo = do
    -- show Pure operations: summing numbers.
    result <- CL.sourceList [1..10] $$ CL.fold (+) 0
    print result
    -- /show

    -- show Exception safe file access: copy a file.
    writeFile "input.txt" "This is a test."
    runResourceT $ CB.sourceFile "input.txt" $$ CB.sinkFile "output.txt"
    readFile "output.txt" >>= putStrLn
    -- /show

    -- show Perform transformations.
    result <- CL.sourceList [1..10] $$ CL.map (+ 1) =$ CL.consume
    print result
    -- /show

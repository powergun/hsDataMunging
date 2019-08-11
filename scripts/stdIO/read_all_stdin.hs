{-
source:
http://learnyouahaskell.com/input-and-output
So what we're essentially doing with that use of forever is
taking the input and transforming it into some output.
That's why we can use getContents to make our program even shorter and better:
-}

import           Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)

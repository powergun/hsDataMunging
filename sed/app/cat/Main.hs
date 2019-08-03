module Main where

-- source:
-- sed implementation in haskell video

-- https://hoogle.haskell.org/?hoogle=interact
-- interact :: (String -> String) -> IO ()
-- cat function is (String -> String)
-- there is also a bytestring version of interact
cat :: String -> String
cat s = s

-- gnu sed source
-- git clone git:://git.savannah.gnu.org/sed.git

-- reuse the last parameters:
-- cat !$
-- see: man history

-- use the original sed to remove empty lines and C comments
-- cat *.{c,h} | sed '/\s*$/d; /\s*\//d' | wc -l

main :: IO ()
main = interact cat

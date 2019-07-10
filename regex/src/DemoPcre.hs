module DemoPcre
  ( demo
  ) where

-- for package details, see:
-- http://hackage.haskell.org/package/regex-pcre

-- see also benchmark:
-- https://wiki.haskell.org/Regular_expressions#regex-pcre
-- pcre is the fastest

-- see also:
-- http://www.pcre.org/
-- cloned to ${WORKSPACE}/c/pcre

import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L

demoExplicitReturnType :: IO ()
demoExplicitReturnType = do
  print "//// demo explicit return type"
  (return ("seta thereisacow=1" =~ "seta?\\s*\\w+=1" :: Bool)) >>=
    print
  -- returns the full match, not the capture group
  (return ("seta thereisacow=1" =~ "seta?\\s*(\\w+)=1" :: String)) >>=
    print
  -- lookahead assertion works! (require fixed length)
  (return ("seta thereisacow=1" =~ "(?<=\\s)\\w+" :: String)) >>=
    print

demoListContext :: IO ()
demoListContext = do
  print "//// demo 'list context' (perl-like)"
  -- 5 matching substrings
  -- why [String] didn't work for me before???
  -- see: https://stackoverflow.com/questions/7636447/raise-no-instance-for-regexcontext-regex-char-string/7637354#7637354
  -- getAllTextMatches() is a function provided by the regex package
  -- to conveniently return the list of string for me
  -- the real type in the context is complicated
  (return (getAllTextMatches $ "there is a cow 1337" =~ "(\\w+)" :: [String])) >>=
    print

-- real world haskell P/240
-- we can obtain quite a lot of information about the context in 
-- which a match occurs. 
-- if we ask for a (String, String, String) tuple, we will get 
-- back the text before the first match, the text of that match 
-- and text that follows it
demoBeforeMatchAfter :: IO ()
demoBeforeMatchAfter = do
  print "//// demo tuple context - before, match, after"
  -- experiment: remove the char a from the pattern and see the 
  -- success message
  -- P/240
  -- if the match fails the entire text is returned as the before
  -- element of the tuple, with the other two elements left empty
  let (before, match, after) = "there is a cow" =~ "[iI][sS]a" :: (String, String, String)
  case length after of
    0 -> print "fail"
    _ -> print $ "success: " ++ after

demoGetCaptureGroups :: IO ()
demoGetCaptureGroups = do
  print "//// demo use tuple context to get capture-groups"
  let (_, _, _, groups) = "0x3243 :mov :eax :ebx ;comment" =~ "(:[\\w]+)+" :: (String, String, String, [String])
  print groups

demoGetStartAndLength :: IO ()
demoGetStartAndLength = do
  print "//// demo get start and length of matches" 
  (return (getAllMatches $ "rther is a cow" =~ "\\w+" :: [(Int, Int)])) >>=
    print

-- NOTE this is the Char8.pack not the EncodeUtf8 version!
-- meaning that this does not work with unicode!
-- example borrowed from:
-- https://gabebw.com/blog/2015/10/11/regular-expressions-in-haskell
demoMatchByteString :: IO ()
demoMatchByteString = do
  print "//// demo match using Char8.ByteString"
  -- NOTE if sut (lhs) is of BC.ByteString, the result type 
  -- must also be BC.ByteString!!
  -- real world haskell P/241 explains this constraint too
  (return (BC.pack "there is acow" =~ BC.pack "is|Is" :: BC.ByteString)) >>=
    print
  -- P/241 this restriction does not apply to the type of the 
  -- regexp we're matching against. It can either be a String
  -- or ByteString, unconstrained by the other types in use
  (return ("there is acow" =~ BC.pack "is|Is" :: String)) >>=
    print

demoMatchLazyByteString :: IO ()
demoMatchLazyByteString = do
  print "/// demo match using Lazy.Char8.ByteString"
  -- NOTE if the subject under text is Lazy ByteString, the
  -- argument and return type MUST be of the same type as well
  sut <- L.readFile "some.txt"
  (return (sut =~ L.pack "is|Is" :: L.ByteString)) >>=
    print

demo :: IO ()
demo = do
  demoExplicitReturnType
  demoListContext
  demoBeforeMatchAfter
  demoGetCaptureGroups
  demoGetStartAndLength
  demoMatchByteString
  demoMatchLazyByteString

module Parse 
  (
    parse
  , identity
  , parseByte
  , parseRawPGM
  )
  where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import Data.Int (Int64)
import Data.Char (chr, isDigit, isSpace)

import PNM

-- real world haskell P/280
-- benefits:
-- A) added ability to track additional information (state bookkeeping)
-- B) avoid pattern matching on the pieces of state that we pass 
-- (leaking private details) and use the accessor functions instead
data ParseState = ParseState {
  string :: L.ByteString
, offset :: Int64
} deriving (Show)

-- P/280
-- we can now look at parsing as a kind of function:
-- it consumes a parsing state and produces both a new parsing 
-- state and some other piece of information.
-- for example:
simpleParse :: ParseState -> (Int, ParseState)
simpleParse state 
  = (0, state)
-- (or even better) to report an error message if parsing fails
betterParse :: ParseState -> Either String (a, ParseState)
betterParse state
  = Left "undefined"

-- P/280
-- in order to future-proof our code, it is best not expose the 
-- implementation of our parser to our users
-- hide the details of our parser type using a newtype declaration
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- newtype definition is just a compile-time wrapper around a 
-- function, so it has no runtime overhead.
-- when we want to use the function, we will apply the runParser
-- accessor
-- if we do not export the Parse value constructor from our module 
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- we can ensure that nobody else will be able to accidentally 
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- create a parser, nor will they be able to inspect its 
-- ^^^^^^^^^^^^^^^
-- internals via pattern matching.
newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}

-- P/280
-- identity parser: turn whatever it is passed into the result of 
-- the parse; resembles the id() function
-- NOTE:
-- try to understand it this way:
-- this function produces a "function object", - parser - , 
-- that has "runParse method"
-- example of execution in GHCI (note how a is substituted by Int)
-- i1 = identity (1 :: Int)
-- :t i1
-- i1 :: Parse.Parse Int
identity :: a -> Parse a
--               ^^^^^^^@see below
identity a = Parse (\s -> Right (a, s))

-- example of execution in GHCI (note how parse i1, produces another
-- function p1)
-- p1 = parse i1
-- :t p1
-- p1 :: L.ByteString -> Either String Int
parse :: Parse a -> L.ByteString -> Either String     a
--       ^^^^^^^@   ````````````initState  ``````err  `` result
parse parser initState
  = case runParse parser (ParseState initState 0) of
--       ^^^^^^^^^^^^^^^call method
--                       ^^^^^^^^^^^^^^^^^^^^^^^ParseState value ctor
      Left err -> Left err
      Right (result, _) -> Right result

parseByte :: Parse Word8
parseByte =
  -- retrieve the current parsing state
  getState ==> \initState ->
  case L.uncons (string initState) of 
    Nothing -> 
      -- terminate parsing and reports an error
      bail "no more input"
    Just (byte, remainder) ->
      -- replace the state
      -- P/282 parseByte function doesn't take the parse state
      -- as an argument
      putState newState ==> \_ ->
        identity byte
      where
        newState = initState { string = remainder, 
                               offset = newOffset }
        newOffset = offset initState + 1

-- real world haskell P/283
-- when reading these functions, recall that the left element of 
-- the tuple is the result of a Parse, while the right is the 
-- current ParseState
-- these functions let us move explicit state handling into the 
-- bodies of only those functions that need it; Many functions 
-- don't need to know what the current state is, and so they will
-- never call getState or putState
-- this lets us write more compact code 
-- work with ParseState using accessors; better encapsulation
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

-- P/283
-- we carefully defined our Parse type to accommodate the 
-- possiblity of failure. The ==> combinator checks for a parse
-- failure and stops parsing if it runs into a failure
bail :: String -> Parse a
-- after we call bail (==>) will successfully pattern match on 
-- the Left ctor that wraps the err msg, and it will not invoke
-- the next parser in the chain; this will cause the error msg
-- to percolate back through the chain of prior callers.
bail err = 
  Parse (\s -> Left ("byte offset " ++ show (offset s) ++ ": " ++ err))

-- P/284
-- recall that the Parse type represents really function inside
-- a wrapper. Since (==>) lets us chain two Parse values to 
-- produce a third, it must return a function, in a wrapper
-- the function just creates a closure to remember the values
-- of firstParser and secondParser
-- a closure will not be unwrapped and applied until we apply 
-- parse
-- real world haskell P/367
-- moving the responsibility for managing the current piece of
-- string out of the individual functions in the chain, and into
-- the function that we used to chain them together
-- we also hid the details of the parsing state in the ParseState
-- type. Even the getState and putState functions don't inspect 
-- the parsing state, so any modification to ParseState will 
-- have no effect on any existing code
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = 
  Parse chainedParser
    where 
      chainedParser initState =
        case runParse firstParser initState of
          -- ````````````````````call method
          Left err -> 
            Left err
          Right (firstResult, newState) ->
            runParse (secondParser firstResult) newState
          -- ``````````````````````````````````call method

-- real world haskell P/290
-- writing a functor instane for parse
-- the function we are fmap-ping should be applied to the current
-- result of a parse and leave the parse state untouched
instance Functor Parse where
  fmap f parser =
    parser ==> \result -> identity (f result)

-- real world haskell P/291
w2c :: Word8 -> Char
w2c = chr . fromIntegral

-- P/291
-- MY NOTE: remember
-- fmap :: Functor f => (a -> b) -> f a -> f b 
-- I have a great function f a, but it only works with type a;
-- I can extend it to work with type b, by wrapping it in a 
-- functor and using a third function a -> b
parseChar :: Parse Char -- this is f b
parseChar = w2c <$> parseByte -- this is f a

-- we can also use functors to write a compact "peek" function
-- this returns Nothing is we are at the end of the input string
-- otherwise it returns the next character without consuming it
-- i.e. it inspects but doesn't disturb the current parsing state
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

-- real world haskell P/292
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []

parseRawPGM =
  parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
  assert (header == "P5") "invalid raw header" ==>&
  parseNat  ==> \width   -> skipSpaces ==>&
  parseNat  ==> \height  -> skipSpaces ==>&
  parseNat  ==> \maxGrey ->
  parseByte ==>&
  parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where
    notWhite = (`notElem` " \r\n\t")

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

-- chains parsers but the righthand side ignores the result from 
-- the left
(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f =
  p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

-- abort parsing with a useful error message if the property is
-- false
assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState ==> \st ->
  let n' = fromIntegral n
      (h, t) = L.splitAt n' (string st)
      st' = st { offset = offset st + L.length h, string = t}
  in putState st' ==>&
     assert (L.length h == n') "end of input" ==>&
     identity h

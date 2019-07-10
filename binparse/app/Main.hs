module Main where

import qualified Data.ByteString.Lazy as L 
import Control.Applicative ((<$>))
import Data.Char

import PNM
import Parse

demoNaiveParsing :: IO ()
demoNaiveParsing = do
  print "//// demo naive parsing"
  -- how to read baboon.pgn binary file lazily (to be compatible
  -- with the parser's type)
  -- see:
  -- https://stackoverflow.com/questions/49015418/haskell-reading-entire-lazy-bytestring/49016161#49016161
  -- image file downloaded from
  -- https://people.sc.fsu.edu/~jburkardt/data/pgmb/pgmb.html
  bs <- L.readFile("baboon.pgm")
  print $ parseP5 bs
  print $ parseP5_ bs

demoIdentityParsers :: IO ()
demoIdentityParsers = do
  print "//// demo parsers"
  bs <- L.readFile("baboon.pgm")
  print $ parse (identity 1)     bs  -- it can even be undef for identity() 
  print $ parse (identity "foo") bs
  --                parser       initState
  --                       (ParseState L.ByteString)

demoParseByte :: IO ()
demoParseByte = do
  bs <- L.readFile("baboon.pgm")
  print "//// demo parseByte()"
  -- observe the first byte in this image file using 
  -- xxd -p baboon.pgm to
  -- the first byte is 0x50 which is indeed 80
  print $ parse parseByte bs

demoParseAsFunctor :: IO ()
demoParseAsFunctor = do
  bs <- L.readFile("baboon.pgm")
  print "//// demo parse as functor"
  -- real world haskell P/290
  -- verify that parse function is a legit functor, by checking 
  -- whether it follows two fundamental functor rules 
  -- rule 1. id rule
  --         try it on a parse that ought to fail
  print $ parse parseByte L.empty
  print $ parse (id <$> parseByte) L.empty
  -- same as below; recall the fmap applies the f function to
  -- the "container" - the result is a combined parser
  -- print $ parse (fmap id parseByte) L.empty
  --         try it on a parse that ought to succeed
  print $ parse (id <$> parseByte) bs

  -- rule 2. being composable
  -- NOTE parseByte yields Word8; I need to call fromIntegral() 
  -- make it a Num to participate further computation
  -- Actual type: Parse.Parse GHC.Word.Word8
  print $ parse ((chr . (+ 1) . fromIntegral) <$> parseByte) bs

demoParseRawPGM :: IO ()
demoParseRawPGM = do
  print "//// demo parseRawPGM()"
  bs <- L.readFile("baboon.pgm")
  print $ parse parseRawPGM bs

main :: IO ()
main = do
  demoNaiveParsing
  demoIdentityParsers
  demoParseByte
  demoParseAsFunctor
  demoParseRawPGM

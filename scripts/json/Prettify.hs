module Prettify
  ( Doc
  , empty
  , char
  , text
  , line
  , (<+>)  -- must be nested in ()!!!
  , double
  ) where

-- TODO: P/166 

-- real world haskell P/165
-- export the name of the type but none of its value ctors
-- this will prevent modules that use Doc type from creating
-- and pattern matching against Doc values

-- instead, to create a Doc a user of Prettify will call a 
-- function that we provide
-- NOTE: recall the GOF factory pattern

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
-- encapsulate the creation logic in the factory function
text "" = Empty
text s = Text s

-- factory function does not have to be a 1:1 mapping with
-- the original value ctors
double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

-- P/165
-- define a custom operator-like function using <+> symbols
-- see also functions/infix
(<+>) :: Doc -> Doc -> Doc
(<+>) Empty y = y
(<+>) x Empty = x
(<+>) x y = Concat x y

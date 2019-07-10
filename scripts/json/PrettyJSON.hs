module PrettyJSON
  (

  ) where

-- TODO: P/166 - P/169

import Numeric

import Prettify

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <+> x <+> char right

oneChar :: Char -> Doc
oneChar c = case c `lookup` simpleEscapes of
  Just r -> text r
  Nothing | mustEscape c -> hexEscape c
          |otherwise -> char c
  where
    mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith toEscaped "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where
    toEscaped a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u"
  <+> text (replicate (4 - length h) '0')
  <+> text h
  where
    h = showHex x ""

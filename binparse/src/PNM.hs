module PNM
  ( parseP5
  , parseP5_
  , Greymap(..)
  ) where

-- real world haskell P/276
-- we use ByteString type to store our graymap data because it is
-- compact
-- since the Header of a PGM file is ascii text but its body
-- is binary we import both the text and binary oriented ByteString
-- modules

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
  greyWeight :: Int
, greyHeight :: Int
, greyMax :: Int
, greyData :: L.ByteString
} deriving (Eq)

-- P/276
-- because our Show instance intentionally avoids printing the 
-- bitmap data, there is no point in writing a Read instance, as 
-- we can not reconstruct a valid Greymap from the result of Show
instance Show Greymap where
  show (Greymap w h m bs) = 
    "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m
      ++ " " ++ (show $ L.length bs)

-- if parse succeeds, it will return a single parsed Greymap, 
-- along with the string that remains after parsing. That 
-- residual string will be available for future parses
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                    case getBytes 1 s4 of
                      Nothing -> Nothing
                      Just (_, s5) ->
                        case getBytes (width * height) s5 of
                          Nothing -> Nothing
                          Just (bitmap, s6) ->
                            Just (Greymap width height maxGrey bitmap, s6)

-- real world haskell P/278
-- every step in the ladder of parseP5 deconstructs a Maybe value
-- and either fails or passes the unwrapped result to a function
-- we haven't provided a fixity declaration for >>? so it defaults 
-- to infixl 9
-- lift associative, strongest operator precedence
-- a >>? b >>? c will be evaluated from left to right as (a >>? b) >>? c
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v  -- f being (a -> Maybe b)

-- real world haskell P/366
-- we had to incrementally consume pieces of a string as we parsed 
-- it. This forced us to pass the current value of the string down
-- our chain of Maybe, wrapped up in a tuple. Each function in 
-- the chain put a result into one element of the tuple and the 
-- unconsumed remainder of the string into the other
parseP5_ :: L.ByteString -> Maybe(Greymap, L.ByteString)
parseP5_ s =
  matchHeader (L8.pack "P5") s >>?
  \s -> skipSpace ((), s) >>?
  (getNat . snd) >>?
  skipSpace >>?
  \(width, s) -> getNat s >>?
  skipSpace >>?
  \(height, s) -> getNat s >>?
  \(maxGrey, s) -> getBytes 1 s >>?
  (getBytes (width * height) . snd) >>?
  \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) =
  Just (a, L8.dropWhile isSpace s)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
  | prefix `L8.isPrefixOf` str
    = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise
    = Nothing

-- nat: natural number
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s =
  case L8.readInt s of
    Nothing -> Nothing
    Just (num, rest)
      | num <= 0 -> Nothing
      | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str =
  let count = fromIntegral n
      both@(prefix, _) = L.splitAt count str
  in if L.length prefix < count
     then Nothing
     else Just both

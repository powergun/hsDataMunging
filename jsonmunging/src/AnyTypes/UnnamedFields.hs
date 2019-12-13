{-# LANGUAGE OverloadedStrings #-}
module AnyTypes.UnnamedFields
  ( demo
  )
where

import           Data.Traversable
import qualified Data.Text                     as T
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as HM
import           Control.Monad                  ( mapM_ )
import           Data.Maybe                     ( fromJust )

data Referer = Referer
  { domain :: String
  , pathAccesses :: [(String, Int)]
  } deriving (Show)

-- use HashMap
-- take fromList [...] produced by parseJSON p,
-- compose a function that:
-- -- extract the list in "fromList [...]",
-- -- for each element in this list, which is a key-value pair (tuple),
-- -- take the key (1st of the tuple) as "domain" field,
-- -- take the value (2nd of the tuple) as the pathAccesses field
-- -- for pathAccesses field, which is also in the form of fromList [...]
-- -- run toList to extract the list
-- use map to run this function on the original fromList [...]
parseReferers :: Value -> Parser [Referer]
parseReferers p =
  map (\(domain, accesses) -> Referer domain (HM.toList accesses))
    .   HM.toList
    <$> parseJSON p

-- a more performant version that avoid Text <-> String conversion
-- NOTE `for` comes from Data.Traversable
parseReferers' :: Value -> Parser [Referer]
parseReferers' = withObject "Referers" $ \o ->
  for (HM.toList o) $ \(domain, referer) -> do
    accesses <- HM.toList <$> parseJSON referer
    let accesses' = map (\(page, n) -> (T.unpack page, n)) accesses
    return $ Referer { domain = T.unpack domain, pathAccesses = accesses' }

demo :: IO ()
demo = do
  print "/// unnamed fields"
  txt <- BL.readFile "./testdata/arbitrary.json"
  let referers  = parseMaybe parseReferers =<< (decode txt :: Maybe Value)
      referers' = parseMaybe parseReferers' =<< (decode txt :: Maybe Value)
      process :: [Referer] -> IO ()
      process = mapM_ $ \r -> do
        print $ "<" ++ domain r ++ ">"
        mapM_ print $ pathAccesses r

  process . fromJust $ referers
  process . fromJust $ referers'

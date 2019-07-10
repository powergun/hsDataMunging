module Barcode
  ( runTests
  ) where

-- TODO: finish this chapter!!
-- real world haskell P/310

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 
  10 - (sum products `mod` 10)
  where
    products = mapEveryOther (*3) (reverse ds)
    
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f =
  zipWith ($) (cycle [f, id])

runTests :: IO ()
runTests = do
  print "//// mapEveryOther()"
  print $ mapEveryOther (+10) [1, 2, 3, 4]

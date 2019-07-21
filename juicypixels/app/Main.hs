module Main where

-- source
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

import           Codec.Picture
import           Data.ByteString.Lazy as BL
import           Test.QuickCheck

instance Arbitrary PixelRGB8 where
  arbitrary = PixelRGB8 <$> arbitrary <*> arbitrary <*> arbitrary

genImage :: Gen (Image PixelRGB8)
genImage = do
  f <- arbitrary
  (x, y) <- arbitrary `suchThat` ( \(x,y) -> x > 0 && y > 0 )
  return $ generateImage f x y

main :: IO ()
main = do
  img <- generate $ resize 1000 genImage
  BL.writeFile "/var/tmp/sut/qc.png" $ encodePng img


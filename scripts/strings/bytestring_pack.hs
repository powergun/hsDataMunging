#!/usr/bin/env stack runghc

-- real world haskell P/235
-- demo L.pack

import qualified Data.ByteString.Lazy as L

demoPack :: IO ()
demoPack = do
  print "//// demo pack"
  -- NOTE L.pack is a pure function hence "return"
  (return $ L.pack [0x33, 0x34, 0x35, 0x46]) >>= print

demoPackRoundTrip :: IO ()
demoPackRoundTrip = do
  print "//// demo pack-unpack roundtrip"
  let sut = [0x33, 0x34, 0x35, 0x46]
      lstr = L.pack sut
  print $ (L.unpack lstr) == sut

main :: IO ()
main = do
  demoPack
  demoPackRoundTrip

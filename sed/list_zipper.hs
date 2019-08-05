#!/usr/bin/env stack runghc

import           Control.Monad
import qualified Data.List.Zipper as Z

demoCreation :: IO ()
demoCreation = do
  let lz = Z.fromList ["there", "is", "a", "cow"]
      le = Z.empty :: Z.Zipper String
  print lz
  print le

demoPush :: IO ()
demoPush = do
  putStrLn
    "//////// demo push ///////////////////////////////////////"
  let lz = Z.fromList ["there", "is", "a", "cow"]
  print (Z.cursor lz)
  let lz' = Z.push "seta" (Z.push "1337" lz) -- pushleft()
  print (Z.cursor lz)
  print (Z.cursor lz')
  print lz
  print lz'
  print (Z.toList lz')

demoDelete :: IO ()
demoDelete = do
  putStrLn
    "//////// demo delete /////////////////////////////////////"
  let lz = Z.fromList ["there", "is", "a", "cow"]
      lz' = Z.delete (Z.delete lz)
  print (Z.cursor lz')
  print lz'

demoEndp :: IO ()
demoEndp = do
  putStrLn
    "//////// demo endp ///////////////////////////////////////"
  let le = Z.empty :: Z.Zipper String
      lz = Z.push "there" le
  print (Z.endp le)
  print (Z.endp lz)

main :: IO ()
main = do
  demoCreation
  demoPush
  demoDelete
  demoEndp

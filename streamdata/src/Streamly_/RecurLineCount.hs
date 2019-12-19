{-# LANGUAGE OverloadedStrings #-}

module Streamly_.RecurLineCount (demo) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad      (forM_, forever)
import           Data.Either
import qualified Data.List          as L
import qualified Data.Map           as M
import qualified Data.Text
import qualified Data.Text.IO
import           Streamly
import           Streamly.Prelude   (nil, (|:))
import qualified Streamly.Prelude   as S
import qualified System.FilePath    as SF

safeReadFile :: FilePath -> IO (Either IOException Data.Text.Text)
safeReadFile = try . Data.Text.IO.readFile

doReadFile :: FilePath -> IO (String, Int)
doReadFile filename = do
    ret <- safeReadFile filename
    either returnEmpty returns ret
  where
    returnEmpty _ = return ("", 0)
    returns s = do
      let ext = SF.takeExtension filename
          lc =  length . Data.Text.lines $ s
      return (ext, lc)

demo :: IO ()
demo = do
    filenames <- return . lines =<< getContents
    l <- S.toList $ asyncly $ foldMap (S.yieldM . doReadFile) filenames
    report l
  where
    emptyDict :: M.Map String Int
    emptyDict = M.empty
    doFold mp elem = M.insertWith (+) (fst elem) (snd elem) mp
    createDict = L.foldl doFold emptyDict
    createSortedList = L.reverse . (L.sortOn snd) . M.toList . createDict
    printEach :: (String, Int) -> IO ()
    printEach t = do
      let padding = "                  "
          l = take 12 $ (fst t) ++ padding
          r = L.reverse . take 12 . L.reverse . (padding ++ ) . show . snd $ t
      putStrLn $ l ++ r
    report l = forM_ (L.take 10 . createSortedList $ l) printEach

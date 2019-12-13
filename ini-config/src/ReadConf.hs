{-# LANGUAGE OverloadedStrings #-}
module ReadConf
    ( demo
    )
where

import           Data.Ini
import           Data.Either

demo :: IO ()
demo =
    either print print $ parseIni "[SERVER]\nport: 6667\nhostname: localhost"

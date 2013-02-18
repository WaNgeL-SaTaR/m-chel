{-# LANGUAGE OverloadedStrings #-}


module Config (myLogin, myPass, myBase, myDesign, webPort,webPass) where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8


myLogin, myPass :: ByteString
-- | Set your login
myLogin = "admin"
-- | Set your pass
myPass = "123"


webPass = "555"

myBase  = "novokshonov1"
myDesign = "m-chel"

webPort = 7003

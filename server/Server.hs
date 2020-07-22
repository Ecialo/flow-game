{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network

server :: IO ()
server = runTCPServer (serverSettings 4000 "*") $ \app ->
  runConduit 
  $ appSource app 
  .| awaitForever (liftIO . BS.putStr)

main :: IO ()
main = server

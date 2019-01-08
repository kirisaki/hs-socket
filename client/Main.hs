{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Monad
import Data.ByteString
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "localhost" "8080"
    bracket (open addr) close talk
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
    talk sock = do
      replicateM_ 3 $ sendAll sock "1234567890"

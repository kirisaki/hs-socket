{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Control.Monad
import Control.Concurrent
import Control.Exception.Safe

main :: IO ()
main = do
  soc <- serveSocket
  sendAll soc "GET http:/v1.39/containers/json"
  print =<< Network.Socket.ByteString.Lazy.getContents soc
  
serveSocket :: IO Socket
serveSocket = do
    soc <- socket AF_UNIX Stream defaultProtocol
    connect soc (SockAddrUnix "/var/run/docker.sock")
    return soc

acceptLoop :: Socket -> IO ()
acceptLoop soc = forever $ do
    (conn, addr) <- accept soc
    forkIO $ echoLoop conn

echoLoop :: Socket -> IO ()
echoLoop conn = do
    sequence_ $ repeat $ do
      str <- recv conn 4096
      send conn str
    `catch` (\(SomeException e) -> return ())
    `finally` close conn

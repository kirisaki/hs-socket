module Main where

import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
    soc <- serveSocket 8080
    listen soc 5
    acceptLoop soc `finally` close soc

serveSocket :: PortNumber -> IO Socket
serveSocket port = do
    soc <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "0.0.0.0"
    bind soc (SockAddrInet port addr)
    return soc

acceptLoop :: Socket -> IO ()
acceptLoop soc = forever $ do
    (conn, addr) <- accept soc
    forkIO $ echoLoop conn

echoLoop :: Socket -> IO ()
echoLoop conn = do
    sequence_ $ repeat $ do
      (str, _, _) <- recvFrom conn 2
      send conn str
    `catch` (\(SomeException e) -> return ())
    `finally` close conn

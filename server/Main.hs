{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Main where

import Data.Drinkery
import Data.ByteString
import Data.ByteString.Internal
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Exception.Safe
import Data.Attoparsec.ByteString as A
import Network.Socket as S 
import Foreign.Ptr
import GHC.IO.Buffer
import Foreign.Storable
import Data.Word

main :: IO ()
main =  do
  soc <- serveSocket 8080
  listen soc 5
  acceptLoop soc `finally` S.close soc

serveSocket :: PortNumber -> IO Socket
serveSocket port = do
  soc <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr "0.0.0.0"
  bind soc (SockAddrInet port addr)
  return soc

acceptLoop :: Socket -> IO ()
acceptLoop soc = forever $ do
  (soc', _) <- S.accept soc
  buf <- newByteBuffer 4 ReadBuffer
  forkFinally (server soc' buf) (\e -> do
                                print e
                                )

server :: Socket -> Buffer Word8 -> IO ()
server soc buf = withBuffer buf $ \ptr -> socketTap soc ptr +& consumer

consumer :: Sink (Tap () ByteString) IO ()
consumer =
  let
    parser = A.take 10
    parseRepeatedly = do
      i <- consume
      case parse parser i of
        Done "" r ->
          liftIO $ print r
        Done i' r -> do
          leftover i'
          liftIO $ print r
        Partial _ -> do
          i' <- consume
          leftover (i <> i')
        _ ->
          liftIO $ print "nyaan"      
  in do
    parseRepeatedly
    consumer

type SocketTap = Tap () ByteString IO

socketTap :: Socket -> Ptr Word8 -> SocketTap
socketTap soc ptr = repeatTapM' io
  where
    io = do
      len <- recvBuf soc ptr 4
      create len $ \buf -> memcpy buf ptr len


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Main where

import Data.Drinkery as DR
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
import Control.Concurrent.KazuraQueue

bufLen = 2048

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
  forkFinally (serve soc')
    (\e -> do
        S.close soc'
    )

serve :: Socket -> IO ()
serve soc = do
  recvQueue <- newQueue
  recvBuf <- newByteBuffer bufLen ReadBuffer
  withBuffer recvBuf $ \ptr -> inexhaustible (receiverTap soc ptr) +& consumer (A.take 10) recvQueue

data Generator a
  = Yield a
  | Pending
  | Failed ByteString
  deriving (Eq, Show)

consumer :: Parser a -> Queue (Generator a)  -> Sink (Tap () ByteString) IO ()
consumer parser q =
  let
    generate = do
      i <- consume
      case parse parser i of
        Done "" r ->
          return $ Yield r
        Done i' r -> do
          leftover i'
          return $ Yield r
        Partial r -> do
          i' <- consume
          leftover (i <> i')
          return Pending
        _ -> do
          return $ Failed i
  in do
    generate >>= liftIO . writeQueue q
    consumer parser q

receiverTap :: Socket -> Ptr Word8 -> Producer () ByteString IO ()
receiverTap soc ptr =
  let
    io = do
      len <- recvBuf soc ptr bufLen
      create len $ \buf -> memcpy buf ptr len
  in do
    produce =<< liftIO io
    receiverTap soc ptr


{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putStrLn)
import Data.Attoparsec.ByteString
import Network.Socket 
import Data.ByteString.Lazy
import Control.Monad
import Control.Concurrent
import Control.Exception.Safe
import Foreign.Storable
import System.IO
import Control.Monad.Managed
import GHC.IO.Buffer
import Foreign.Storable
import Data.Word
import Data.Char
import Data.ByteString.Unsafe

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
  (soc', _) <- accept soc
  buf <- newByteBuffer 4 ReadBuffer
  forkFinally (echo soc' buf) (\e -> do
                                print e
                                )
  where
    parser = takeTill (== 0x20)
    ignitor = parse parser ""
    echo :: Socket -> Buffer Word8 -> IO ()
    echo soc buf = withBuffer buf $ \ptr -> loop ptr ignitor
      where
        loop ptr result = do
          len <- recvBuf soc ptr 4
          str <- unsafePackCStringFinalizer ptr len (return ())
          let newResult = feed result str
          print newResult
          loop ptr newResult
      




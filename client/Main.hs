{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.ByteString hiding (replicate)
import Data.ByteString.Internal
import Network.Socket
import System.Random
import Foreign.Ptr
import GHC.IO.Buffer
import Foreign.Storable
import Foreign.ForeignPtr

bufLen = 2048

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "localhost" "8080"
    replicateM_ 10000 $ do
      threadDelay =<< randomRIO (0, 1000)
      void . forkIO $ bracket (open addr) close talk
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
      let PS bs _ _ = pack $ replicate bufLen 0x00
      withForeignPtr bs $ \ptr -> do
        threadDelay =<< randomRIO (0, 1000)
        replicateM_ 1024 $ sendBuf sock ptr bufLen 


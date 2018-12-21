{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Data.Attoparsec.ByteString
import Network.Socket 
import Data.ByteString hiding (putStrLn)
import Control.Monad
import Control.Concurrent
import Control.Exception.Safe
import Foreign.Storable
import Control.Monad.Managed
import Control.Monad.State.Strict
import GHC.IO.Buffer
import Foreign.Storable
import Data.Word
import Data.Char
import Data.ByteString.Unsafe
import Pipes as P
import qualified Pipes.Prelude as P
import Pipes.Attoparsec hiding (parse)
import Pipes.Parse
import Foreign.Ptr
import Control.Monad.IO.Class

main :: IO ()
main =  do
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
    parser = Data.Attoparsec.ByteString.take 10
    parser' :: Pipes.Parse.Parser ByteString IO (Maybe (Either ParsingError (Int, ByteString)))
    parser' = parseL parser
    echo :: Socket -> Buffer Word8 -> IO ()
    echo soc buf = withBuffer buf $ \ptr -> do
      forever $ do
         runStateT (runEffect $ loop ptr >-> consumer) (parse parser "")
      where
        consumer = do
          val <- P.await
          liftIO $ print val
        feedRepeatly :: Result ByteString -> ByteString -> Producer ByteString (StateT (Result ByteString) IO) ()
        feedRepeatly result str =
          case feed result str of
            Done left val -> do
              if left == ""
                then return ()
                else feedRepeatly (parse parser "") left
              lift $ put (parse parser left)
              P.yield val
            Partial r ->
              lift $ put (Partial r)
            Fail _ _ _ -> do
              liftIO $ print "nyaan"
              lift $ put (parse parser "")
        loop :: Ptr Word8 -> Producer ByteString (StateT (Result ByteString) IO) ()
        loop ptr = do
          len <- liftIO $ recvBuf soc ptr 4
          str <- liftIO $ unsafePackCStringFinalizer ptr len (return ())
          result <- lift $ get
          feedRepeatly result str
          loop ptr
        loop' ptr = do
          len <- lift $ recvBuf soc ptr 4
          str <- lift $ unsafePackCStringFinalizer ptr len (return ())
          P.yield str
          loop' ptr
      



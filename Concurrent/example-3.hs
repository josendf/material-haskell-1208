{-# LANGUAGE DeriveDataTypeable #-}

import Helpers

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Either
import Data.Typeable
import Network
import Prelude hiding (catch)
import System.IO
import Text.Printf
import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- Our Async API:

data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
   m <- newEmptyMVar
   t <- forkIO (do r <- try action; putMVar m r)
   return (Async t m)

wait :: Async a -> IO (Either SomeException a)
wait (Async t var) = readMVar var

cancel :: Async a -> IO ()
cancel (Async t var) = throwTo t ThreadKilled

-----------------------------------------------------------------------------

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

main = withSocketsDo $ do
  putStrLn "Start..."
  putStrLn ""

  as <- mapM (async.http) sites

  forkIO $ do
     hSetBuffering stdin NoBuffering
     forever $ do
        c <- getChar
        when (c == 'q') $ mapM_ cancel as

  rs <- mapM wait as
  printf "%d/%d finished\n" (length (rights rs)) (length rs)

  putStrLn ""
  putStrLn "Done."

 where
   http url = do
     (page, time) <- timeit $ getURL url
     printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Either
import Data.Typeable
import Prelude hiding (catch)
import System.IO
import Text.Printf
import qualified Data.ByteString as B

data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
   m <- newEmptyMVar
   t <- forkIO (do r <- try action; putMVar m r)
   return (Async t m)

wait :: Async a -> IO (Either SomeException a)
wait (Async t var) = readMVar var

cancel :: Async a -> IO ()
cancel (Async t var) = killThread t

doWork :: Char -> IO ()
doWork c = forever $ putChar c

main :: IO ()
main = do
  putStrLn "Start.."
  putStrLn ""

  hSetBuffering stdout NoBuffering

  threads <- mapM (async . doWork) ['A','B','C']

  threadDelay (10^6)

  mapM_ cancel threads

  mapM_ wait threads

  putStrLn ""
  putStrLn "Done."

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import System.IO
import System.IO.Unsafe

{--
  The lowest-level communication abstraction in Concurrent Haskell is the
  MVar, whose interface is given below:

  data MVar a
    newEmptyMVar :: IO (MVar a)
    newMVar      :: a -> IO (MVar a)
    takeMVar     :: MVar a -> IO a
    putMVar      :: MVar a -> a -> IO ()
--}

main :: IO ()
main = do
  putStrLn "Start..."
  putStrLn ""

  hSetBuffering stdout NoBuffering

  threadA <- forkChild ( forever ( putChar 'A') )

  threadB <- forkChild ( forever ( putChar 'B') )

  threadDelay (10^6)

  killThread threadA

  killThread threadB

  waitForChildren

  putStrLn ""
  putStrLn "Done."

{--
  In a standalone GHC program, only the main thread is required to terminate
  in order for the process to terminate. Thus all other forked threads will
  simply terminate at the same time as the main thread (the terminology for
  this kind of behaviour is "daemonic threads").

  If you want the program to wait for child threads to finish before exiting,
  you need to program this yourself.

  A simple mechanism is to have each child thread write to an MVar when it
  completes, and have the main thread wait on all the MVars before exiting.

  A better method is to keep a global list of all child threads which we
  should wait for at the end of the program:
--}

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
       putMVar children ms
       takeMVar m
       waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())

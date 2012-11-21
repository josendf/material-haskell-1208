module Main where

import Control.Concurrent
import Control.Monad
import System.IO

{--
 forkIO :: IO () -> IO ThreadId
 forkIO takes a computation of type IO () as its argument; that is, a com-
 putation in the IO monad that eventually delivers a value of type (). The
 computation passed to forkIO is executed in a new thread that runs con-
 currently with the other threads in the system. If the thread has effects,
 those effects will be interleaved in an indeterminate fashion with the effects
 from other threads.
--}

main :: IO ()
main = do
  putStrLn "Start..."
  putStrLn ""

  hSetBuffering stdout NoBuffering

  threadA <- forkIO ( forever ( putChar 'A') )

  threadB <- forkIO ( forever ( putChar 'B') )

  threadDelay (10^6)

  killThread threadA

  killThread threadB

  putStrLn ""
  putStrLn "Done."
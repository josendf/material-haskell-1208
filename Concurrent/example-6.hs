module Main where
import Helpers

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
   t <- forkIO $ try action >>= putMVar m
   return (Async t m)

wait :: (Show a, Integral a) => Async a -> IO a
wait (Async t var) = do
     r <- readMVar var
     case r of
          Left e  -> do putStrLn $ show (e::SomeException)
                        return 0
          Right v -> do putStrLn $ "Thread: "
                                ++ (show t ++ (" result: " ++ show v))
                        return v

cancel :: Async a -> IO ()
cancel (Async t var) = killThread t

doWork :: [Int] -> IO Int
doWork batch = return $ sum batch

toBeDone :: [Int]
toBeDone = [1..100]

makeBatches :: Int -> [Int] -> [[Int]]
makeBatches size work = go work []
  where
  go [] x  = x
    go xs ys = let (p, s) = splitAt size xs
               in go s (p : ys)

main :: IO ()
main = do
  putStrLn "Start.."
  putStrLn ""

  hSetBuffering stdout NoBuffering

  threads <- mapM (async . doWork) (makeBatches 20 toBeDone)

  -- mapM_ cancel threads
  results <- mapM wait threads

  putStrLn $ "Total: " ++ show (sum results)

  putStrLn ""
  putStrLn "Done."

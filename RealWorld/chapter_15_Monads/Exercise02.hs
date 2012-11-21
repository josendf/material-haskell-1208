{-
  Chapter 15 Monads
  Real World Haskell
  runhaskell ./Exercise02.hs 
  start-process cmd /K,'runhaskell ./Exercise02.hs'
-}
module Main where
import System.Directory
import System.FilePath

import Control.Monad
import Supply
import System.Random hiding (next)

-- (Just 1,[2,3])
ret1 = runSupply nextSup [1,2,3]

-- ((Just 1,Just 2),[3])
ret2 = runSupply (liftM2 (,) nextSup nextSup) [1,2,3]

-- ((Just 1,Nothing),[])
ret3 = runSupply (liftM2 (,) nextSup nextSup) [1]


randomsIO :: IO [Int]
randomsIO = getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)

ret4 :: IO ()
ret4 = do 
    rs <- randomsIO
    putStrLn (show(head rs))

ret5 :: IO ()
ret5 = do 
    rs <- fmap (runSupply nextSup) randomsIO

    putStrLn (show (fst (rs)))


showTwo :: (Show s, Monad m, MonadSupply s m) => m String
showTwo = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)

main :: IO ()
main = do
    -- currDir <- getCurrentDirectory
    -- let root = currDir </> ".."

    putStrLn ""

    putStrLn "[ret1]"
    putStrLn $ show ret1

    putStrLn "[ret2]"
    putStrLn $ show ret2

    putStrLn "[ret3]"
    putStrLn $ show ret3

    putStrLn "[ret4]"
    ret4

    putStrLn "[ret5]"
    ret5

    putStrLn ""


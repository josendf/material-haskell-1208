{-
  Writer Monad
  http://learnyouahaskell.com/for-a-few-monads-more
-}
module Main where
import Control.Monad
import Control.Monad.Writer



logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Number: " ++ show x])  

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b) 

ret1 = runWriter multWithLog

multWithLog2 :: Writer [String] Int  
multWithLog2 = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Multiply"]  
    return (a*b) 

ret2 = runWriter multWithLog2


greatComDiv :: Int -> Int -> Writer [String] Int 
greatComDiv a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        greatComDiv b (a `mod` b)

ret3 = runWriter (greatComDiv 8 3)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] } 
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  

countDownFast :: Int -> Writer (DiffList String) ()  
countDownFast 0 = do  
    tell (toDiffList ["0"])  
countDownFast x = do  
    countDownFast (x-1)  
    tell (toDiffList [show x]) 

ret4 = runWriter (countDownFast 500000)


countDownSlow :: Int -> Writer [String] ()  
countDownSlow 0 = do  
    tell ["0"]  
countDownSlow x = do  
    countDownSlow (x-1)  
    tell [show x]

ret5 = runWriter (countDownSlow 500000)

{-

  runhaskell ./WriterMonad-03.hs

-}

main::IO()
main = do 
  putStrLn "\n<Inicio>"

  putStrLn "<ret1>"
  putStrLn $ show ret1

  putStrLn "<ret2>"
  putStrLn $ show ret2

  putStrLn "<ret3>"
  mapM_ putStrLn $ snd $ ret3

  putStrLn "<ret4>"
  mapM_ putStrLn $ snd $ ret4


  
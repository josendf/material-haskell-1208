module Main where

import Control.Monad
import Control.Monad.Trans.Writer

logInt :: Int -> Writer [String] Int
logInt x = writer (x, ["Number: " ++ show x])  

multWithLog :: Writer [String] Int
multWithLog = do  
    x <- logInt 3 
    y <- logInt 5 
    return (x * y)

-- A computation with a (result, output) pair
newtype MyWriter a = MyWriter (a, [String])
    deriving Show

instance Monad MyWriter where
    return x = MyWriter(x, [])

    (MyWriter (x, y)) >>= k = MyWriter (x', y ++ y')
        where
            (MyWriter (x', y')) = k x

-- Construct a writer computation from a (result, output) pair
myWriter :: a -> String -> MyWriter a
myWriter x y = MyWriter (x, [y])

-- Unwrap a writer computation as a (result, output) pair.
myRunWriter :: MyWriter a -> (a, [String])
myRunWriter (MyWriter (x, y)) = (x, y)

-- Extract the output from a writer computation.
myExecWriter :: MyWriter a -> [String]
myExecWriter = snd . myRunWriter


myLogInt :: Int -> MyWriter Int
myLogInt x = myWriter x ("Number: " ++ show x) 

myMultWithLog :: MyWriter Int
myMultWithLog = do  
    x <- myLogInt 3 
    y <- myLogInt 5 
    return (x * y)

myMultWithLogDes :: MyWriter Int  
myMultWithLogDes =               -- ^ 
    myLogInt 3 >>= (\ x ->       -- | La expresión se
    myLogInt 5 >>= (\ y ->       -- | resuelve desde dentro hacia fuera 
    return (x * y)))             -- | de los paréntesis

main::IO()
main = do
    putStrLn "Start..."

    putStrLn "Writer example..."
    print (runWriter multWithLog)

    putStrLn "MyWriter example..."
    print (myRunWriter myMultWithLog)

    putStrLn "MyWriter desugared example..."
    print (myRunWriter myMultWithLogDes)

    putStrLn "Done."
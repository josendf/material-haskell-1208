module Main where

import Control.Monad.Trans.State

-- El estado que deseamos connservar
type MyState = (Int, String)

-- El resultado de la operación
type MyResult = Bool


myOp1 :: State MyState MyResult  
myOp1 = do

    -- Leemos el estado
    (x, y) <- get

    -- Modificamos el estado
    put (x + 1, y ++ "+1")

    -- Establecemos el resultado
    myOp2


myOp2 :: State MyState MyResult  
myOp2 = do

    -- Leemos el estado
    (x, y) <- get

    -- Modificamos el estado
    put (x + 1, y ++ "+2")

    -- Establecemos el resultado
    return True

initialState :: MyState
initialState = (0, "") 

main :: IO ()
main = do

    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    print $ runState myOp1 initialState 
    putStrLn ""

    print $ evalState myOp1 initialState 
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
    putStrLn ""

{-# LANGUAGE  GADTs #-}

{-- 
    Generalized Algebraic Datatypes
    http://en.wikibooks.org/wiki/Haskell/GADT 
--} 

module Main where

{-- GADTs
    What we need is a way to restrict the return types of the constructors 
    proper, and that's exactly what generalized data types do. 
--}

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Equ :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2)  = eval e1 + eval e2
eval (Mul e1 e2)  = eval e1 * eval e2
eval (Equ  e1 e2) = eval e1 == eval e2

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    let e1 = (I 1 `Add` I 2) `Mul` I 3
    print $ eval e1
    putStrLn ""

    let e2 = (I 1 `Add` I 2) `Equ` I 3
    print $ eval e2
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
    putStrLn ""



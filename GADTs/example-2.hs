{-# LANGUAGE  GADTs #-}

{-- 
    Generalized Algebraic Datatypes
    http://en.wikibooks.org/wiki/Haskell/GADT 
--} 

module Main where

data Expr = I Int         -- integer constants
          | Add Expr Expr -- add two expressions
          | Mul Expr Expr -- multiply two expressions
          deriving (Eq, Show)
{-- 
 This data type corresponds to an abstract syntax tree.

 An arithmetic term like

  (5 + 1) * 7 
 
 would be represented as

  (I 5 `Add` I 1) `Mul` I 7 :: Expr.

--}

eval :: Expr -> Int
eval (I n)       = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    let e1 = (I 5 `Add` I 1) `Mul` I 7 :: Expr
    print $ eval e1

    putStrLn ""
    putStrLn "Done."
    putStrLn ""

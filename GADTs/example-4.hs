{-# LANGUAGE  GADTs #-}

{-- 
    Generalized Algebraic Datatypes
    http://en.wikibooks.org/wiki/Haskell/GADT 
--} 

module Main where

{-- Phantom types
    The so-called phantom types are the first step towards our goal. 
    The technique is to augment the Expr with a type variable.

    Note that an expression Expr a does not contain a value a at all; that's 
    why a is called a phantom type, it's just a dummy variable. 
--}

data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            deriving (Eq, Show)

{-- 
    By only exporting the smart constructors, the user cannot create
    expressions with incorrect types.
--}
int :: Int -> Expr Int
int = I

bool :: Bool -> Expr Bool
bool = B

add :: Expr Int -> Expr Int -> Expr Int
add = Add

mul :: Expr Int -> Expr Int -> Expr Int
mul = Mul

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    let e1 = (int 5 `add` int 1) `mul` int 7
    -- print $ eval e1
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
    putStrLn ""

{-# LANGUAGE  GADTs #-}

{-- 
    Generalized Algebraic Datatypes
    http://en.wikibooks.org/wiki/Haskell/GADT 
--} 

module Main where

data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Mul Expr Expr
          | Equ Expr Expr 
          deriving (Eq, Show)

eval :: Expr -> Maybe (Either Int Bool)

eval (I n) = Just (Left n)

eval (B n) = Just (Right n)

eval (Add x y) = do
    l <- x'
    r <- y'
    return $ Left (l + r)
  where
    x' = case eval x of
        Just (Left p)  -> Just p 
        _ -> Nothing
    y' = case eval y of
        Just (Left p) -> Just p 
        _ -> Nothing

eval (Mul x y) = do
    l <- x'
    r <- y'
    return $ Left (l * r)
  where
    x' = case eval x of
        Just (Left p) -> Just p 
        _ -> Nothing
    y' = case eval y of
        Just (Left p) -> Just p 
        _ -> Nothing

eval (Equ x y) = do
    l <- x'
    r <- y'
    return $ Right (l == r)
  where
    x' = case eval x of
        Just (Left p) -> Just p 
        _ -> Nothing
    y' = case eval y of
        Just (Left p) -> Just p 
        _ -> Nothing

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    let e1 = (I 5 `Add` I 1) `Mul` I 7 :: Expr
    print $ eval e1
    putStrLn ""

    let e2 = (I 5 `Add` I 2) `Equ` I 7 :: Expr
    print $ eval e2
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
    putStrLn ""

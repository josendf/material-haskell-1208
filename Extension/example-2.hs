{-# LANGUAGE GADTs #-}
module Main where

{-- Software Extension and Integration with Type Classes
    Ralf Lämmel, Klaus Ostermann
--}

-- An open type class for expression forms
class Exp e

-- An evaluation operation
class Exp e => Eval e where
  eval :: e -> Int

-- Concrete expression forms

-- Literal
data Lit = Lit Int

instance Exp Lit

instance Eval Lit where eval (Lit i) = i

-- Addition (GADTs)
data Add where Add :: (Exp e, Eval e) => e -> e -> Add

instance Exp Add

instance Eval Add where eval (Add x y) = eval x + eval y

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Start..."
  putStrLn ""

  let exp1 = Add (Lit 1) (Lit 2)
  let res1 = eval exp1
  print res1

  let exp2 = Add (Lit 3) (Lit 4)
  let res2 = eval exp2
  print res2

  let exp3 = Add exp1 exp2
  let res3 = eval exp3
  print res3

  putStrLn ""
  putStrLn "Done."
  putStrLn ""

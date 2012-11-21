module Main where

{-- Software Extension and Integration with Type Classes
    Ralf Lämmel, Klaus Ostermann
--}

-- An open type class for expression forms
class Exp x

-- An evaluation operation
class Exp x => Eval x where
  eval :: x -> Int

-- Concrete expression forms

-- Literal
data Lit = Lit Int
instance Exp Lit
instance Eval Lit where
  eval (Lit i) = i

-- Addition
data Add a = Add a a
instance (Exp a) => Exp (Add a)
instance (Eval a) => Eval (Add a) where
  eval (Add x y) = eval x + eval y

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

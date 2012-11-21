{-# LANGUAGE   ExistentialQuantification
             , GADTs #-}
module Main where

{--

data ShowBox s = Show s => SB s

heteroList :: [ShowBox s]
heteroList = [SB (), SB 5, SB True]

Couldn't match type `()' with `Bool'
In the first argument of `SB', namely `()'
In the expression: SB ()
In the expression: [SB (), SB 5, SB True]

El tipo de s se debe fijar en la
declaración de la lista  

heteroList :: [ShowBox Bool]
heteroList = [SB True, SB False]


--}


-- Existential Quantification
data ShowBoxE = forall s. Show s => SBE s

heteroListE :: [ShowBoxE]
heteroListE = [SBE (), SBE 5, SBE True]

instance Show ShowBoxE where
  show (SBE s) = show s
 
-- Generalized Algebraic Datatypes
data ShowBoxG where 
    SBG :: Show s => s -> ShowBoxG   

heteroListG :: [ShowBoxG]
heteroListG = [SBG (), SBG 5, SBG True]

instance Show ShowBoxG where
  show (SBG s) = show s

printAll :: Show a => [a] -> IO ()
printAll xs = mapM_ print xs

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    printAll heteroListE
    putStrLn ""

    printAll heteroListG
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
    putStrLn ""

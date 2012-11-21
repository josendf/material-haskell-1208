{-- exercise-02.hs 

> ./exercise-02.sh
> ./exercise-02 1 "+" 2
> ./exercise-02 1 "-" 2
> ./exercise-02 2 "*" 3

Exercise
========
Change the program so it performs a simple arithmetic
operation on the two arguments and prints out the result.

You can use read to convert a string to a number, and show to 
convert a number back into a string.

Play around with different operations.

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

Haskell Report
--------------
Function application is written e1 e2.

Application associates to the left, so the parentheses may be omitted
in (f x) y.

Because e1 could be a data constructor, partial applications of data
constructors are allowed.

The non-strict application operator $ may appear redundant, since ordinary 
application (f x) means the same as (f $ x). 

However, $ has low, right-associative binding precedence, so it sometimes 
allows parentheses to be omitted; for example:

f $ g $ h x = f (g (h x))

--}
module Main where
import System.Environment

main :: IO ()
main = do
        args <- getArgs

        putStrLn (
              (show (getLeft args)) ++ " " ++ 
              (show (getSym args)) ++ " " ++ 
              (show (getRight args)) ++ " = " ++ 
              (show (compute args)) ++ "\n")

        putStrLn (
              (show $ getLeft args) ++ " " ++ 
              (show $ getSym args) ++ " " ++ 
              (show $ getRight args) ++ " = " ++ 
              (show $ compute args) ++ "\n")

       where

          leftArg = 0; oprArg = 1; rightArg = 2

          getArg args n = read (args !! n)::Integer

          getLeft args = getArg args leftArg

          getRight args = getArg args rightArg

          getSym args = args !! oprArg
          
          getOpr symb = case symb of
                        "+" -> (+) 
                        "-" -> (-) 
                        "*" -> (*) 

          compute args =  (getOpr (getSym args)) (getLeft args) (getRight args)




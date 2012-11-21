{-- exercise-01.hs 

> ./exercise-01.sh
> ./exercise-01 1 2

Exercise
========
Change the program so it reads two arguments
from the command line, and prints out a message using both of them.

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

--}
module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("arg0 is: " ++ args !! 0 ++ "\narg1 is: " ++ args !! 1)
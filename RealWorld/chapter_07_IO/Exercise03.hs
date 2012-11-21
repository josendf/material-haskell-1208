{-
  Chapter 7 IO
  Real World Haskell
  runhaskell ./Exercise03.hs
-}
module Main where
import System.IO
import Data.Char(toUpper)

{-
Haskell programmers use hGetContents as a filter quite often. 
They read from one file, do something to the data, and write the result out 
elsewhere.

This is so common that there are some shortcuts for doing it. 

The functions readFile and writeFile are shortcuts for working with
files as strings. They handle all the details of opening files, closing files, 
reading data, and writing data. readFile uses hGetContents internally.
-}

main :: IO ()
main = do 
       inpStr <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inpStr)


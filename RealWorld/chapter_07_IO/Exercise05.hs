{-
  Chapter 7 IO
  Real World Haskell
  runhaskell ./Exercise05.hs < input.txt > output.txt 
  runhaskell ./Exercise05.hs < input.txt
-}
module Main where
import System.IO
import Data.Char(toUpper)

{-
Another common use of interact is filtering.

Let's say that you want to write a program that reads a file and prints out 
every line that contains the character 'u'. Here's how you
might do that with interact:
-}

main :: IO ()
main = interact (unlines . filter (elem 'u') . lines)


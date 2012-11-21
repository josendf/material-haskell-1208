{-
  Chapter 7 IO
  Real World Haskell
  runhaskell ./Exercise04.hs < input.txt > output.txt 
  runhaskell ./Exercise04.hs < input.txt
-}
module Main where
import System.IO
import Data.Char(toUpper)

{-
You learned that readFile and writeFile address the common situation of reading
from one file, making a conversion, and writing to a different file.

There's a situation that's even more common than that: 
  reading from standard input, making a conversion, and
  writing the result to standard output.

For that situation, there is a function called interact.

The type of interact is (String -> String) -> IO ().

That is, it takes one argument: a function of type String -> String.

That function is passed the result of getContents, that is, standard input 
read lazily. 

The result of that function is sent to standard output.
-}

main :: IO ()
main = interact (map toUpper)


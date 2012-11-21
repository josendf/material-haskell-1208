{-
  Chapter 7 IO
  Real World Haskell
  runhaskell ./Exercise02.hs
-}
module Main where
import System.IO
import Data.Char(toUpper)

{-
hGetContents

One novel way to approach I/O is with the hGetContents function.
hGetContents has the type Handle -> IO String.

The String it returns represents all of the data in the file given 
by the Handle, more precisely, it is the entire data from the current position 
of the file pointer to the end of the file.

There is also a shortcut function called getContents that operates on 
standard input.

In a strictly evaluated language, using such a function is often a bad idea.

It may be fine to read the entire contents of a 2 KB file, but if you try to 
read the entire contents of a 500 GB file, you are likely to crash due to 
lack of RAM to store all that data.

But hGetContents is different. The String it returns is evaluated lazily.

At the moment you call hGetContents, nothing is actually read. Data is only 
read from the Handle as the elements (characters) of the list are processed.

As elements of the String are no longer used, Haskell's garbage collector 
automatically frees that memory. All of this happens completely transparently
to you. And since you have what looks like (and, really, is) a pure
String, you can pass it to pure (non-IO) code.
-}

main :: IO ()
main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       let result = processData inpStr
       hPutStr outh result
       hClose inh
       hClose outh

processData :: String -> String
processData = map toUpper

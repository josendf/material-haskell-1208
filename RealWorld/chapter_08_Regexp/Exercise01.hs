{-
  Chapter 7 IO
  Real World Haskell
  runhaskell ./Exercise01.hs "impsum" < input.txt > output.txt 
  runhaskell ./Exercise01.hs "impsum" < input.txt
-}
module Main where
import System.IO
import System.Environment
import Text.Regex.Posix ((=~))

main :: IO ()
main = do
        args <- System.Environment.getArgs
        let pat = args !! 0
        if (length args) < 1 then error "First argument must be a regex pattern."
        else interact (unlines . filter (matches pat) . lines)

matches :: String -> String -> Bool
matches pat text = text =~ pat

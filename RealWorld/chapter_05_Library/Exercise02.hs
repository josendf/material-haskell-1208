{-
  Chapter 5 Writing a Library
  Working with JSON
  Real World Haskell
-}
module Main where
import SimpleJSON

ret1 = JString "Hello"

ret2 = getString ret1


{-
  runhaskell ./Exercise02.hs
-}
main :: IO ()
main = do
  putStrLn ""
  putStrLn "[Inicio]"

  putStrLn "[ret1]"
  putStrLn $ show ret1

  putStrLn "[ret2]"
  putStrLn $ show ret2


  putStrLn "[Fin]"
  putStrLn ""

  
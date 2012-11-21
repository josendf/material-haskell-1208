{-
  Chapter 5 Writing a Library
  Working with JSON
  Real World Haskell
-}
module Main where
import Data.List (intercalate)
import SimpleJSON
import PrettyJSON

ret1 = JObject [
    ("aString", JString "Abc"),
    ("aNumber", JNumber 123),
    ("aBool", JBool True),
    ("aNull", JNull)
    ]    

putJValue :: JValue -> IO ()
putJValue v = putStrLn ( show $ renderJValue v)


{-
  runhaskell ./Exercise03.hs
-}
main :: IO ()
main = do
  putStrLn ""
  putStrLn "[Inicio]"

  putStrLn "[ret1]"
  putJValue ret1

  putStrLn "[Fin]"
  putStrLn ""

  
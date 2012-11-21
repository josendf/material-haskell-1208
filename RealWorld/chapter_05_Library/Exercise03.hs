{-
  Chapter 5 Writing a Library
  Working with JSON
  Real World Haskell
-}
module Main where
import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s) = show s

renderJValue (JNumber n) = show n

renderJValue (JBool True) = "true"

renderJValue (JBool False) = "false"

renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " $ map renderPair ps
          renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " $ map renderJValue vs

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

ret1 = JObject [
    ("aString", JString "Abc"),
    ("aNumber", JNumber 123),
    ("aBool", JBool True),
    ("aNull", JNull)
    ]    

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

  
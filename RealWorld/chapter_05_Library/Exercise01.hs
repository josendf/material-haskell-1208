{-
  Chapter 5 Writing a Library
  Working with JSON
  Real World Haskell
-}
module Main where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

ret1 = JString "Hello"

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

ret2 = getString ret1

getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v = v == JNull

{-
  runhaskell ./Exercise01.hs
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

  
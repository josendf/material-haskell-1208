import Control.Applicative

miFunc :: String -> String
miFunc x = "valor: " ++ x

obtVal :: String
obtVal = "a"

obtValCol :: Maybe String
obtValCol = Just "a"

obtValColNull :: Maybe String
obtValColNull = Nothing

obtValColDb :: Either String (Maybe String)
obtValColDb = Right (Just "a")

obtValColDbNull :: Either String (Maybe String)
obtValColDbNull = Right Nothing

obtValColDbErr :: Either String (Maybe String)
obtValColDbErr = Left "error: connection"

obtValColDbTbl :: [Either String (Maybe String)]
obtValColDbTbl = [obtValColDb, obtValColDbNull, obtValColDbErr]


main :: IO ()
main = do
    
    putStrLn "Ejemplo 1"
    print $ miFunc obtVal
    putStrLn ""
    
    putStrLn "Ejemplo 2"
    print $ fmap miFunc obtValCol
    putStrLn ""
    
    putStrLn "Ejemplo 3"
    print $ fmap miFunc obtValColNull
    putStrLn ""
    
    putStrLn "Ejemplo 4"
    print $ (fmap . fmap) miFunc obtValColDb
    putStrLn ""

    putStrLn "Ejemplo 5"
    print $ (fmap . fmap) miFunc obtValColDbNull
    putStrLn ""

    putStrLn "Ejemplo 6"
    print $ (fmap . fmap) miFunc obtValColDbErr
    putStrLn ""

    putStrLn "Ejemplo 7"
    print $ (fmap . fmap . fmap) miFunc obtValColDbTbl
    putStrLn ""

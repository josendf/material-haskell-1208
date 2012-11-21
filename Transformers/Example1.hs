import Control.Applicative

-- Una función pura 
miFunc :: String -> String
miFunc x = "valor: " ++ x

-- Un valor
obtVal :: String
obtVal = "a"

-- Un valor en:
--  1. un contexto de posible indeterminación (Maybe)
obtValCol :: Maybe String
obtValCol = Just "a"

-- Un valor indeterminado (Nothing)
obtValColNull :: Maybe String
obtValColNull = Nothing

-- Un valor en:
--   1. un contexto de posible indeterminación (Maybe)
--   2. un contexto de posible fallo (Either)
obtValColDb :: Either String (Maybe String)
obtValColDb = Right (Just "a")

-- Un valor indeterminado (Nothing) sin fallo (Right)
obtValColDbNull :: Either String (Maybe String)
obtValColDbNull = Right Nothing

-- Un error
obtValColDbErr :: Either String (Maybe String)
obtValColDbErr = Left "error: connection"

-- Varios valores en:
--   1. un contexto de posible indeterminación (Maybe)
--   2. un contexto de posible fallo (Either)
--   3. un contexto de elección (List)
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

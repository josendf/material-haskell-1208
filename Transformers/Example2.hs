{-
-}
module Main where

import Control.Monad(guard, mzero)
import Control.Monad.Trans(lift, liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

{--
    Para combinar diferentes monads usamos un monad
    como base, puede ser IO, [], Identity, etc. y sobre
    esta base apilamos los monads adicionales.

    En este caso usamos como base IO.
--}


myGetLine :: IO String
myGetLine = return "abc"

{--
    Apilamos Maybe String sobre IO
    
    When using combined monads created by the monad transformers, we avoid 
    having to explicitly manage the inner monad types, resulting in clearer, 
    simpler code. Instead of creating additional do-blocks within the 
    computation to manipulate values in the inner monad type, we can use lifting 
    operations to bring functions from the inner monad into the combined monad.
    
    Recall the liftM family of functions which are used to lift non-monadic 
    functions into a monad. Each monad transformer provides a lift function that 
    is used to lift a monadic computation into a combined monad.
    
    The MonadTrans class is defined in Control.Monad.Trans and provides the 
    single function lift. The lift function lifts a monadic computation in the 
    inner monad into the combined monad.
     
--}
type MyContext = String
type MyResult  = String
type MyError   = String
type MyLog     = String

func1 :: MyContext -> MaybeT IO MyResult
func1 c = do 
    l <- liftIO myGetLine
    if l == c then return l
    else mzero

func2 :: MyContext -> ErrorT MyError IO MyResult
func2 c = do 
    l <- liftIO myGetLine
    if l == c then return l
    else throwError "match error"

-- Debemos aplicar lift una vez por cada nivel de apilamiento
-- func3 :: ReaderT (ErrorT IO)
--  ReaderT -
--   ErrorT lift
-- IO, que es la base, requiere un solo lift
func3 :: ReaderT MyContext (ErrorT MyError IO) MyResult
func3 = do
    c <- ask 
    l <- liftIO myGetLine
    if l == c then return l
    else lift $ throwError "match error"

-- Debemos aplicar lift una vez por cada nivel de apilamiento
-- func4 :: ErrorT (ReaderT (WriterT IO))
-- ErrorT  -
--  ReaderT lift
--    WriterT lift . lift
-- IO, que es la base, requiere un solo lift
func4 :: ErrorT MyError (ReaderT MyContext (WriterT [MyLog] IO)) MyResult
func4 = do
    (lift . lift) $ tell ["log func4"]
    c <- lift ask 
    l <- liftIO myGetLine
    if l == c then return l
    else throwError "match error"

-- El orden de apilamiento es importante
-- en este caso perdemos el log en caso de error
func4Bad :: WriterT [MyLog] (ReaderT MyContext (ErrorT MyError IO)) MyResult
func4Bad = do
    tell ["log func4"]
    c <- lift ask 
    l <- liftIO myGetLine
    if l == c then return l
    else (lift . lift) $ throwError "match error"   


main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    putStrLn "1."
    r1 <- runMaybeT (func1 "abc")
    print r1
    putStrLn ""

    putStrLn "2."
    r2 <- runMaybeT (func1 "###")
    print r2
    putStrLn ""

    putStrLn "3."
    r3 <- runErrorT (func2 "abc")
    print r3
    putStrLn ""

    putStrLn "4."
    r4 <- runErrorT (func2 "###")
    print r4        
    putStrLn ""

    putStrLn "5."
    r5 <- runErrorT (runReaderT func3 "abc")
    print r5 
    putStrLn ""

    putStrLn "6."
    r6 <- runErrorT (runReaderT func3 "###")
    print r6 
    putStrLn ""

    putStrLn "7."
    r7 <- runWriterT (runReaderT (runErrorT func4) "abc ") 
    print r7 
    putStrLn ""

    -- El orden de apilamiento es importante
    -- en este caso NO perdemos el log en caso de error
    putStrLn "8."
    r8 <- runWriterT (runReaderT (runErrorT func4) "###") 
    print r8 
    putStrLn ""

    -- El orden de apilamiento es importante
    -- en este caso SI perdemos el log en caso de error
    putStrLn "9."
    r9 <- runErrorT (runReaderT (runWriterT func4Bad)  "###") 
    print r9 
    putStrLn ""


    putStrLn ""
    putStrLn "Done."
    putStrLn ""
    
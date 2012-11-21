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

-- Los accesors runXXX recuperan el monad encapsulado
-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

func1 :: String -> MaybeT IO String
func1 c = do 

    -- The lift function lifts a monadic computation in the 
    -- inner monad into the combined monad.

    -- liftIO :: IO a -> m a
    -- Lift a computation from the IO monad.

    -- instance MonadIO IO where
    --    liftIO = id

    -- instance (MonadIO m) => MonadIO (MaybeT m) where
    --    liftIO = lift . liftIO

    -- instance MonadTrans MaybeT where
    --  lift = MaybeT . liftM Just

    {--
        Después de aplicar liftIO se aplicará el bind de MaybeT:
        
        x >>= f = MaybeT $ do
            v <- runMaybeT x
            case v of
                Nothing -> return Nothing
                Just y  -> runMaybeT (f y)
    --}

    -- Lift a computation from the argument monad to the constructed monad.
    -- lift :: Monad m => m a      -> t m a
    --                   IO String -> MaybeT IO String

    -- liftIO myGetLine
    -- liftIO (IO String)
    -- liftIO (IO String)
    -- liftIO (IO String)
    -- (lift . liftIO) (IO String)
    -- ((MaybeT . liftM Just) . id) (IO String)

    l <- liftIO myGetLine
    
    -- return = lift                            . return
    --          lift                            . (a -> m a)
    --          lift                            . (String -> IO String)
    --          (IO String -> MaybeT IO String) . (String -> IO String)
    
    if l == c then return l
    
    
    -- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
    else MaybeT (return Nothing)
    --            m (Maybe a)
    --            IO (Maybe String)


main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    -- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
    --
    -- r1                 <- runMaybeT (func1 "abc")
    -- m (Maybe a)                       MaybeT m a
    -- IO (Maybe String)                 MaybeT IO String
    putStrLn "1."
    r1 <- runMaybeT (func1 "abc")
    print r1
    putStrLn ""

    putStrLn "2."
    r2 <- runMaybeT (func1 "###")
    print r2
    putStrLn ""



    putStrLn ""
    putStrLn "Done."
    putStrLn ""
    
module Main where

import Control.Monad.Trans.Reader


readerExample1 :: Reader Int String
readerExample1 = do
    a <- ask
    return (show (a + 1))


data MyParams = MyParams {
        prmWidth :: Int,
        prmTop :: Int,
        prmLeft:: Int
    } deriving (Eq, Show) 

{-
  Very often, it's the case that you have a lot of functions that all need to 
  use the same values. 
  Typically, you handle this situation by adding a new parameter to all
  your functions which represents the state.
  
  This works just fine, but it can get really ugly.

  Every single function gets a new parameter, which must be threaded through to 
  every related function that it ever calls.

  This causes visual clutter, particularly for functions that don't actually use
  the parameter themselves but just pass it on.
-}

myFunc1 :: MyParams -> String -> String
myFunc1 prms text = take maxW text 
  where
    maxW = prmWidth prms 


{-
  Note that the Reader monad is partially applied here.
  The result, WithParams, is a new monad that adds a parameter of 
  type MyParams to any code that runs inside it.
-}

type WithParams = Reader MyParams

myFunc2 :: String -> WithParams String
myFunc2 text = do

    -- ask :: Monad m => ReaderT r m r
    -- Fetch the value of the environment.
    prms <- ask

    let maxW = prmWidth prms 

    return (take maxW text)


myFunc3 :: String -> WithParams String
myFunc3 text = do

    -- asks :: Monad m => (r -> a) -> ReaderT r m a
    -- Retrieve a function of the current environment.
    maxW <- asks prmWidth

    return (take maxW text)


myFunc4 :: String -> WithParams String
myFunc4 text =

    -- local :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
    -- Execute a computation in a modified environment 
    local (\ prms -> prms {prmWidth = 2}) (myFunc3 text)


myFunc5 :: String -> WithParams ((Int, Int), String)
myFunc5 text = do

    left <- asks prmLeft
    top  <- asks prmTop

    newText <- myFunc3 text

    return ((left, top), newText)


main :: IO ()
main = do

    putStrLn "Start..."

    let myParams = MyParams {prmWidth=4, prmTop=3, prmLeft=2}

    putStrLn (runReader readerExample1 1)

    putStrLn (myFunc1 myParams "ABCDEFG")

    putStrLn (runReader (myFunc2 "ABCDEFG") myParams)

    putStrLn (runReader (myFunc3 "ABCDEFG") myParams)

    putStrLn (runReader (myFunc4 "ABCDEFG") myParams)

    print (runReader (myFunc5 "ABCDEFG") myParams)

    putStrLn "Done."

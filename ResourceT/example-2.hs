{-# LANGUAGE  OverloadedStrings #-}

{-
https://github.com/snoyberg/conduit
-}

module Main where

import Control.Exception
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)
import Debug.Trace

{-

ResourceT

So our first issue to address is to create a new way to deal with resource 
allocation. We represent this as a monad transformer, ResourceT. It works as 
follows:

You can register a cleanup action, which will return a ReleaseKey.

If you pass your ReleaseKey to the release function, your action will be called 
automatically, and your action will be unregistered.

When the monad is exited (via runRelease), all remaining registered actions will 
be called.

All of this is provided in an exception-safe manner.

For example, you would be able to open a file handle, and then register an 
action to close the file handle. In your code, you would call release on your 
ReleaseKey as soon as you reach the end of the contents you are streaming. If 
that code is never reached, the file handle will be released when the monad 
terminates.

-}

openRes :: String -> IO String 
openRes loc = do 
    putTraceMsg $ "open(" ++ loc ++ ")"  
    return loc 

closeRes :: String -> IO () 
closeRes loc = do
    putTraceMsg ("close(" ++ loc ++")")  

failOp :: String -> IO ()
failOp op = do
    putTraceMsg $ "fail(" ++ op ++ ")"  
    throw $ ErrorCall ("error: " ++ op)

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    r1 <- openRes "file-1"
    closeRes r1
    putStrLn ""

    runResourceT $ do
        (relKey, res) <- withIO (openRes "file-2") closeRes

        -- runResourceT garantiza la llamada a closeRes
        -- aunque se produzcan excepciones

        lift (failOp "read")

        -- Solamente es necesario llamar a release
        -- si queremos anticipar la liberación del recurso 
        --  release relKey


    putStrLn ""
    putStrLn "Done."
    putStrLn ""

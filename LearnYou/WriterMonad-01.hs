{-
  Writer Monad
  http://learnyouahaskell.com/for-a-few-monads-more
-}

module Main where
import Control.Monad
import Data.Monoid

{-
http://learnyouahaskell.com/for-a-few-monads-more

Let's make a function that takes a value with an attached log, that is, an 
(a,String) value and a function of type a -> (b,String) and feeds that value 
into the function. 

We'll call it applyLog. 

But because an (a,String) value 
doesn't carry with it a context of possible failure, but rather a context of an 
additional log value, applyLog is going to make sure that the log of the 
original value isn't lost, but is joined together with the log of the value that 
results from the function. 
-}


applyLog1 :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog1 (x,log) f = let (y,newLog) = f x in (y,log ++ newLog) 

{-
Now our applyLog can work for any monoid. We have to change the type to reflect 
this, as well as the implementation, because we have to change ++ to mappend
-}

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog) 

ret1 = applyLog (1,"One") (\x -> (2,"Two"))
ret2 = (1,"One") `applyLog` (\x -> (2,"Two"))
ret3 = (1,"One") `applyLog` (\x -> (2,"Two")) `applyLog` (\x -> (3,"Three"))

{-

  runhaskell ./WriterMonad-01.hs

-}

main::IO()
main = do 
  putStrLn "\n<Inicio>"
  putStrLn "<ret1>"
  putStrLn $ show ret1
  putStrLn "<ret2>"
  putStrLn $ show ret2
  putStrLn "<ret3>"
  putStrLn $ show ret3

  
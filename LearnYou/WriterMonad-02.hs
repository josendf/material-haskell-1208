{-
  Writer Monad
  http://learnyouahaskell.com/for-a-few-monads-more
-}
module Main where
import Control.Monad
import Data.Monoid hiding (Product)

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog) 

type Product = String  
type Price = Sum Int 

addExtra :: Product -> (Product,Price)
addExtra "PC" = ("Monitor", Sum 100)
addExtra "Laptop" = ("Bag", Sum 10) 
addExtra _ = ("No extra", Sum 0) 

ret1 = ("PC",Sum 400) `applyLog` addExtra

ret2 = ("Laptop",Sum 600) `applyLog` addExtra

ret3 = ("Notebook",Sum 300) `applyLog` addExtra

{-

  runhaskell ./WriterMonad-02.hs

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

  
{-
  Chapter 9 IO
  Real World Haskell
  runhaskell ./Exercise03.hs 
  Powershel: & runhaskell ./Exercise03.hs
-}
module Main where

{-

A Haskell function can only accept a single argument and return one parameter.

Haskell allows us to pretend that several arguments are passed, but this form 
is treated as a series of nested lambda functions.

    f x y = x + y

is treated as

    f = \x -> \y -> x + y    -- (1)

This treatment is true for lambda functions as well 

    \x y -> x + y 

is treated as 
    
    \x -> \y -> x + y

This allows us to treat type declaration as left-associative, that is 
    
    f :: Int -> Int -> Int 

is actually 
    
    f :: (Int -> (Int -> Int))
    
which fits exactly to (1) above:
    
    f has no arguments, but is returning a function which accepts Int.
    This function in turn returns a function which accepts another Int, and 
    returns an Int.

    This means that if we want to return a function from a function, we 
    don't have to do anything special, since that's Haskell's "default" mode.

This also means that given the type declaration 

    f :: Int -> Int -> Int 
    
We can write f's implementatoin ("equation") with 0, 1 or 2 parameters.

If one or two parameter are specified, the compiler will generate the 
necessary lambdas to comply with the form 

f :: (Int -> (Int -> Int))

f = \x -> \y -> x + y

f x = \y -> x + y       -- \x -> is generated

f x y = x + y           -- \x -> \y is generated

But in each of these cases, the function application appearing to accept 
two parameters will compile successfully, since it will always be 
translated to the first form, e.g.

f 4 5 --> (\x -> (\y -> x + y) 5 ) 4

-}

func1 :: String -> String -> String -> String
func1 x y z = x ++ y ++ z

res1 = func1 "A" "B" "C"


func2 :: String -> String -> String -> String
func2 = (\x -> (\y -> (\z -> x ++ y ++ z)))

res2 = func2 "A" "B" "C"

res3 = ((func2 "A") "B") "C"


func1_1 x = func1 x 
func1_2 y = func1_1 y
func1_3 z = func1_2 z

res4 = func1_3 "A" "B" "C"


main :: IO ()
main = do

    putStrLn "" 

    putStrLn "[res1]" 
    putStrLn $ show res1 

    putStrLn "[res2]" 
    putStrLn $ show res2 

    putStrLn "[res3]" 
    putStrLn $ show res3 

    putStrLn "[res4]" 
    putStrLn $ show res4 

    putStrLn "" 


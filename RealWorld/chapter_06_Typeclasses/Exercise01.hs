{-
  Chapter 6 Typeclasses
  Real World Haskell
-}
module Main where

{-

if we know what isEqual or isNotEqual would return,
we know how to figure out what the other function would return, for all types.
Rather than making users of the typeclass define both functions for all types, 
we can provide default implementations for them. 
Then, users will only have to implement one function.

People implementing this class must provide an implementation of at least 
one function.

They can implement both if they wish, but they will not be required to.

While we did provide defaults for both functions, each function depends on the 
presence of the other to calculate
an answer.

If we don't specify at least one, the resulting code would be an endless loop.
Therefore, at least one function must always be implemented.

-}

data Color = Red | Green | Blue

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

class MyEq a where
    isEqual :: a -> a -> Bool
    isEqual x y = not (isNotEqual x y)

    isNotEqual :: a -> a -> Bool
    isNotEqual x y = not (isEqual x y)


instance MyEq Color where
    isEqual Red Red     = True
    isEqual Green Green = True
    isEqual Blue Blue   = True
    isEqual _ _         = False
    

instance MyEq Char where
    isEqual x y = x == y

instance MyEq Int where
    isEqual x y = x == y

instance MyEq Integer where
    isEqual x y = x == y

instance MyEq Float where
    isEqual x y = x == y

instance MyEq Double where
    isEqual x y = x == y

instance MyEq Bool where
    isEqual x y = x == y

{-
instance (MyEq a) => MyEq [a] where
    isEqual []     []     = True
    isEqual []     (_:_)  = False
    isEqual (_:_)  []     = False
    isEqual (x:xs) (y:ys) = case isEqual x y of
                                True  -> isEqual xs ys
                                other -> other
-}

instance (MyEq a) => MyEq [a] where
    isEqual []     []     = True
    isEqual (x:xs) (y:ys) = (isEqual x  y) && (isEqual xs ys)
    isEqual _xs    _ys    = False

instance (MyEq a, MyEq b) => MyEq (a,b) where
    isEqual (u,v) (x,y)   = (isEqual u x) && (isEqual v y)
 

ret1 = isEqual Red Green

ret2 = isEqual "Abc" "Abc"

ret3 = isEqual (1::Int) (2::Int)

ret4 = (1::Int) `isEqual` (2::Int)

ret5 = x `isEqual` y
    where x = 1::Int
          y = 2::Int

ret6 = isEqual ("A",["b","c","d"]) ("A",["b","c","d"])

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

  putStrLn "[ret3]"
  putStrLn $ show ret3

  putStrLn "[ret6]"
  putStrLn $ show ret6

  putStrLn "[Fin]"
  putStrLn ""

  
{-# LANGUAGE TemplateHaskell #-}

{- Lenses
  
  See
  https://github.com/ekmett/lens/wiki/Overview
  http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf
  
  Lenses are composable functional references.
  They allow you to access and modify data potentially very deep
  within a structure.
-}

module Main where

import Control.Lens


{-
  Lenses provide us with two operations:

         view :: Lens' a b -> a -> b

         set :: Lens' a b -> b -> a -> a

  So we can view a lens as a pair of a getter and a setter that
  are in some sense compatible.
-}
example1 :: IO ()
example1 = do

    print obj
    print $ view _1 obj
    print $ view _2 obj

    let obj' = set _2 "people" obj
    print obj'
    print $ view _1 obj'
    print $ view _2 obj'

  where
    obj = ("hello", "world")
                
{-
  We define infix operators to make working with lenses
  feel more imperative:

       (^.) :: a -> Lens' a b -> b
  
       (.~) :: Lens' a b -> b -> a -> a

  With these you can now use lenses like field accessors.
-}
example2 :: IO ()
example2 = do

    print obj
    print $ obj ^. _1
    print $ obj ^. _2

    let obj' = obj & _1 .~ "people"
    print obj'
    print $ view _1 obj'
    print $ view _2 obj'

  where
    obj = ("hello", "world")


{-
  Here we generate some lenses automatically for a record type Location
  using Template Haskell. 
  See https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/basic-lensing

  The underscores in the record names _degree, _minute, etc.
  are a Control.Lens convention for generating TH.
-}

data Arc      = Arc      {_degree   :: Int, _minute    :: Int, _second :: Int} deriving(Show)
data Location = Location {_latitude :: Arc, _longitude :: Arc} deriving(Show)

-- This is a TH splice, it just creates some functions for us automatically based on the record
-- functions in 'Location'.
$(makeLenses ''Location)
$(makeLenses ''Arc)

{-
  The result of this TH splice is the creation of two lenses,
  one corresponding to each field of the record.

  latitude  :: Lens' Location Arc
  longitude :: Lens' Location Arc
-}

example3 :: IO()
example3 = do

    print obj
    print $ obj ^. latitude
    print $ obj ^. longitude

    let obj' = obj & latitude . degree .~ 10
    print obj'
    print $ obj' ^. latitude
    print $ obj' ^. longitude

  where
    obj = Location (Arc 1 2 3) (Arc 4 5 6)


main :: IO ()
main = do
  putStrLn "# Start"


  putStrLn "## Example1"
  example1

  putStrLn "## Example2"
  example2

  putStrLn "## Example3"
  example3

  putStrLn "# Done"

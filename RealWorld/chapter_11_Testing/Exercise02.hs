{-
  Chapter 11 Testing
  Real World Haskell
  runhaskell ./Exercise02.hs 
  start-Process cmd /K,'runhaskell ./Exercise02.hs'
-}
module Main where
import System.Directory
import System.FilePath
import System.Random
import Data.List
import Data.Monoid
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen
import SimpleJSON

{-
QuickCheck encourages an approach to testing where the developer specifies 
invariants that should hold for any data we can throw at the code. To test the 
pretty printing library, then, we'll need a source of input data. To do this, we 
take advantage of the small combinator suite for building random data that 
QuickCheck provides via the Arbitrary class. The class provides a function, 
arbitrary, to generate data of each type, and with this we can define our data 
generator for our custom data types.

  elements :: [a] -> Gen a
  choose   :: Random a => (a, a) -> Gen a
  oneof    :: [Gen a] -> Gen a

The function elements, for example, takes a list of values, and returns a 
generator of random values from that list. choose and oneof we'll use later. 


-}

instance Arbitrary Doc where
    arbitrary = do
        n <- choose (1,6) :: Gen Int
        case n of
             1 -> return Empty

             2 -> do x <- arbitrary
                     return (Char x)

             3 -> do x <- arbitrary
                     return (Text x)

             4 -> return Line

             5 -> do x <- arbitrary
                     y <- arbitrary
                     return (Concat x y)

             6 -> do x <- arbitrary
                     y <- arbitrary
                     return (Union x y)

{-

That was fairly straightforward, and we can clean it up some more by using the 
oneof function, whose type we saw earlier, to pick between different generators 
in a list (we can also use the monadic combinator, liftM to avoid naming 
intermediate results from each generator)

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]
-}


ret1 :: [Doc]
ret1 = unGen arbitrary (System.Random.mkStdGen 2) 10 ::[Doc]


prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

prop_char c   = char c   == Char c

prop_text s   = text s   == if null s then Empty else Text s

prop_line     = line     == Line

prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

prop_punctuate s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]

        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys


prop_text_id x =
    pretty 100 (Text x) == x

prop_append x y =
    pretty n (Text x <> Text y) == x ++ y
  where
    n = 100

prop_mempty_id x =
    mempty `mappend` x == x
  &&
    x `mappend` mempty == (x :: Doc)


{-
*** Failed! Falsifiable (after 2 tests)
-}
prop_fsep xs = fsep xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds


{-
*** Failed! Falsifiable (after 4 tests)
-}
prop_concat_append x y =
    pretty n (x <> y) == pretty n x ++ pretty n y
  where
    n = 100


main :: IO ()
main = do
    -- currDir <- getCurrentDirectory
    -- let root = currDir </> ".."

    putStrLn ""
    putStrLn "[ret1]"
    putStrLn $ show ret1

    putStrLn ""
    putStrLn "[tests]"

    quickCheck prop_empty_id
    quickCheck prop_char
    quickCheck prop_text
    quickCheck prop_line
    quickCheck prop_double
    quickCheck prop_hcat
    quickCheck prop_punctuate
    quickCheck prop_text_id
    quickCheck prop_append
    quickCheck prop_mempty_id
    -- quickCheck prop_fsep
    -- quickCheck prop_concat_append
    
    
    putStrLn ""
    

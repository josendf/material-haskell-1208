{-
  Chapter 11 Testing
  Real World Haskell
  runhaskell ./Exercise01.hs 
  start-Process cmd /K,'runhaskell ./Exercise01.hs'
-}
module Main where
import System.Directory
import System.FilePath
import System.Random
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Gen

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

{-
One useful invariant to start with, and one that comes up in a lot of purely 
functional code, is idempotency — applying a function twice has the same result 
as applying it only once. For our sort routine, a stable sort algorithm, this 
should certainly be true, or things have gone horribly wrong! This invariant can 
be encoded as a property simply

We'll use the QuickCheck convention of prefixing test properties with prop_ to 
distinguish them from normal code.
-}

prop_idempotent xs = qsort (qsort xs) == qsort xs

{-
QuickCheck library comes with a set of data generators for all the basic Haskell 
data types. QuickCheck uses the Arbitrary typeclass to present a uniform 
interface to (pseudo-)random data generation with the type system used to 
resolve which generator to use. QuickCheck normally hides the data generation 
plumbing, however we can also run the generators by hand to get a sense for the 
distribution of data QuickCheck produces.
-}

ret1 :: [Bool]
ret1 = unGen arbitrary (System.Random.mkStdGen 2) 10 ::[Bool]

{-
The list sorting function should certainly have a number of interesting 
properties that tie it to other list operations. For example: the first element 
in a sorted list should always be the smallest element of the input list. 

This property will only hold for non-empty lists. QuickCheck, thankfully, 
comes with a full property writing embedded language, so we can specify more 
precisely our invariants, filtering out values we don't want to consider. For 
the empty list case, we really want to say: if the list is non-empty, then the 
first element of the sorted result is the minimum. This is done by using the 
(==>) implication function, which filters out invalid data before running the 
property
-}
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

{-
QuickCheck generates test data like this and passes it to the property of our 
choosing, via the quickCheck function. The type of the property itself 
determines which data generator is used. quickCheck then checks that for all the 
test data produced, the property is satisfied. Now, since our idempotency test 
is polymorphic in the list element type, we need to pick a particular type to 
generate test data for, which we write as a type constraint on the property. To 
run the test, we just call quickCheck with our property function, set to the 
required data type.
-}


main :: IO ()
main = do
    -- currDir <- getCurrentDirectory
    -- let root = currDir </> ".."

    putStrLn ""
    putStrLn "[ret1]"
    putStrLn $ show ret1

    putStrLn ""
    putStrLn "[tests]"

    quickCheck (prop_idempotent :: [Integer] -> Bool)

    quickCheck (prop_minimum :: [Integer] -> Property)

    quickCheck (prop_ordered :: [Integer] -> Bool)

    quickCheck (prop_permutation :: [Integer] -> Bool)

    quickCheck (prop_maximum :: [Integer] -> Property)

    quickCheck (prop_append :: [Integer] -> [Integer] -> Property)

    putStrLn ""



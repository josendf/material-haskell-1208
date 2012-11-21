{-
  Chapter 12 Array and Map
  Real World Haskell
  runhaskell ./Exercise01.hs 
  start-process cmd /K,'runhaskell ./Exercise01.hs'
-}
module Main where
import System.Directory
import System.FilePath

import Data.Array (Array(..), (!), bounds, elems, indices,
                   ixmap, listArray, accumArray)
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)

{-
-}

{-

(<$>) :: Functor f => (a -> b) -> f a -> f b
An infix synonym for fmap.

-}

leftList = ["0001101", "0011001", "0010011", "0111101", "0100011",
            "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList = map complement <$> leftList
    where complement '0' = '1'
          complement '1' = '0'

rightList1 = map (map complement) leftList
    where complement '0' = '1'
          complement '1' = '0'


{-

The Data.Array module's listArray function populates an array from a list. It 
takes as its first parameter the bounds of the array to create; the second is 
the values with which to populate it.

Notice that we have to specify the lower and upper bounds of the array. These 
bounds are inclusive, so an array from 0 to 2 has elements 0, 1, and 2.

Haskell arrays are not mutable. This means that to “modify” a single array 
element, a copy of the entire array is made, with that single element set to 
its new value. Clearly, this approach is not a winner for performance. 

The mutable array is a building block for another ubiquitous imperative data 
structure, the hash table. 

At a single stroke, then, immutable arrays have eliminated two canonical 
imperative data structures from our toolbox: mutables arrays and hash tables.

Arrays are somewhat less useful in pure Haskell code than in many other 
languages. Still, many array codes only update an array during a build phase, 
and subsequently use it in a read-only manner.

This is not the calamitous situation that it might seem, though. Arrays and hash 
tables are often used as collections indexed by a key, and in Haskell we use 
trees for this purpose.

The attraction of a tree to a functional programmer is cheap modification. We 
don't break the immutability rule: trees are immutable just like everything 
else. However, when we modify a tree, creating a new tree, we can share most of 
the structure of the tree between the old and new versions. 

Haskell's standard libraries provide two collection types that are implemented 
using balanced trees behind the scenes: Data.Map for key/value pairs, and 
Data.Set for sets of values. As we'll be using Data.Map in the sections that 
follow, we'll give a quick introduction to it below. Data.Set is sufficiently 
similar that you should be able to pick it up quickly.

-}

listToArray :: [a] -> Array Int a
listToArray xs = listArray (lowBound,uppBound) xs
    where lowBound = 0
          uppBound = (length xs) - 1

ret1 = listToArray leftList

{-

Once an array is constructed, we can use the (!) operator to access its elements 
by index.

-}

ret2 = ret1 ! 2

{-

The bounds function returns a tuple describing the bounds that we used to create 
the array. The indices function returns a list of every index. We can use these 
to define some useful folds, since the Data.Array module doesn't define any fold 
functions itself.

-}

-- Strict left fold, similar to foldl' on lists.
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
    where go s (j:js) = let s' = f s (a ! j)
                        in s' `seq` go s' js
          go s _ = s

btrFoldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
btrFoldA f s = foldl' f s . elems

ret3 = foldA (+) 0 arr
    where arr = listToArray [1,2,3]

ret5 = btrFoldA (+) 0 arr
    where arr = listToArray [1,2,3]


-- Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists.
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) (listToArray (tail (elems a)))


btrFoldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
btrFoldA1 f = foldl1 f . elems

ret4 = foldA1 (+) arr
    where arr = listToArray [1,2,3]

ret6 = btrFoldA1 (+) arr
    where arr = listToArray [1,2,3]

{-

The accumArray function deals with repeated indices in the association list 
using an accumulating function which combines the values of associations with 
the same index.

For example, given a list of values of some index type, hist 
produces a histogram of the number of occurrences of each index within a 
specified range

accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e

-}

hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]

ret7 = hist (1,3) [1,2,3,1,3,3]


main :: IO ()
main = do
    -- currDir <- getCurrentDirectory
    -- let root = currDir </> ".."

    putStrLn ""
    
    putStrLn ""
    putStrLn "[leftList]"
    putStrLn $ show leftList

    putStrLn ""
    putStrLn "[rightList]"
    putStrLn $ show rightList
    
    putStrLn ""
    putStrLn "[rightList1]"
    putStrLn $ show rightList1
    
    putStrLn ""
    putStrLn "[ret1]"
    putStrLn $ show ret1

    putStrLn ""
    putStrLn "[ret2]"
    putStrLn $ show ret2

    putStrLn ""
    putStrLn "[ret3]"
    putStrLn $ show ret3

    putStrLn ""
    putStrLn "[ret4]"
    putStrLn $ show ret4

    putStrLn ""
    putStrLn "[ret5]"
    putStrLn $ show ret5

    putStrLn ""
    putStrLn "[ret6]"
    putStrLn $ show ret6

    putStrLn ""
    putStrLn "[ret7]"
    putStrLn $ show ret7

    putStrLn ""


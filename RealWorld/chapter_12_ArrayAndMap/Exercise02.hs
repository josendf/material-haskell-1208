{-
  Chapter 12 Array and Map
  Real World Haskell
  runhaskell ./Exercise02.hs 
  start-process cmd /K,'runhaskell ./Exercise02.hs'
-}
module Main where
import System.Directory
import System.FilePath
import Control.Monad (forM_)
import Control.Applicative ((<$>))

-- import qualified Data.Map as M

import Data.Map

{-

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


{-

To create an empty map, we use empty.

For a map containing one key/value pair, we use singleton.

fromList builds a map from a list of key/value pairs.

-}

ret1 :: Map () ()
ret1 = Data.Map.empty

ret2 :: Map String Int
ret2 = Data.Map.singleton "one" 1

ret3 :: Map String Int
ret3 = fromList [("one",1),("two",2),("three",3)]


{-

Since the implementation of Map is abstract, we can't pattern match on 
Map values. Instead, it provides a number of lookup functions, of which two 
are particularly widely used. 

M.lookup :: (Ord k, Monad m) => k -> M.Map k a -> m a

Most often, the type parameter m in the result is Maybe. In other words, if the 
map contains a value for the given key, lookup will return the value wrapped in 
Just. Otherwise, it will return Nothing.

-}

ret4 = Data.Map.lookup "two" mp
    where mp = fromList [("one",1),("two",2),("three",3)]

ret5 = case match "two" of 
        Just v -> show v
        Nothing -> "not found"
    where mp = fromList [("one",1),("two",2),("three",3)]
          match k = Data.Map.lookup k mp

ret6 = case match "zero" of 
        Just v -> show v
        Nothing -> "<not found>"
    where mp = fromList [("one",1),("two",2),("three",3)]
          match k = Data.Map.lookup k mp

{-

The findWithDefault function takes a value to return if the key isn't in the 
map.

findWithDefault :: Ord k => a -> k -> Map k a -> a

The expression (findWithDefault def k map) returns the value at key k or returns 
default value def when the key is not in the map.
 
-}

ret7 = findWithDefault 0 "zero" mp
    where mp = fromList [("one",1),("two",2),("three",3)]
          
{-

To add a key/value pair to the map, the most useful functions are insert and 
insertWith'. The insert function simply inserts a value into the map, 
overwriting any matching value that may already have been present. 

insert :: Ord k => k -> a -> Map k a -> Map k a

insertWith' :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k aSource

Insert with a function, combining new value and old value. insertWith f key 
value mp will insert the pair (key, value) into mp if key does not exist in the 
map. If the key does exist, the function will insert the pair (key, f new_value 
old_value).

The insertWith' function takes a further combining function as its argument. If 
no matching key was present in the map, the new value is inserted verbatim. 
Otherwise, the combining function is called on the new and old values, and its 
result is inserted into the map.

As the tick at the end of its name suggests, insertWith' evaluates the combining 
function strictly. This allows you to avoid space leaks. While there exists a 
lazy variant (insertWith without the trailing tick in the name), it's rarely 
what you actually want.

-}

ret8 = Data.Map.insert "four" 4 mp
    where mp = fromList [("one",1),("two",2),("three",3)]

ret9 = Data.Map.insertWith' (++) "K2" "xyz-" mp
    where mp = fromList [("K1","abc"),("K2","def"),("K3","ghi")]

main :: IO ()
main = do
    -- currDir <- getCurrentDirectory
    -- let root = currDir </> ".."

    putStrLn ""
    
    putStrLn ""
    putStrLn "[ret1]"
    putStrLn $ showTree ret1

    putStrLn ""
    putStrLn "[ret2]"
    putStrLn $ showTree ret2

    putStrLn ""
    putStrLn "[ret3]"
    putStrLn $ showTree ret3

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
    putStrLn "[ret8]"
    putStrLn $ showTree ret8

    putStrLn ""
    putStrLn "[ret9]"
    putStrLn $ showTree ret9

    putStrLn ""


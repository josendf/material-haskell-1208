module Library (splitLines, asInt) where
import Data.Char (digitToInt)

{- 
The Prelude defines a function named break that we can use to partition a list into two
parts. It takes a function as its first parameter. That function must examine an element of
the list and return a Bool to indicate whether to break the list at that point. The break
function returns a pair, which consists of the sublist consumed before the predicate
returned True (the prefix) and the rest of the list (the suffix).
-}
splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r':rest)      -> splitLines rest
    ('\n':rest)      -> splitLines rest
    _                -> []
  
isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'


asInt :: String -> Int
loop :: Int -> String -> Int
asInt xs = loop 0 xs
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

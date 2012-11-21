{-
  Chapter 6 Typeclasses
  Real World Haskell
-}
module Main where


data Color = Red | Green | Blue

instance Eq Color where
    (==) Red Red     = True
    (==) Green Green = True
    (==) Blue Blue   = True
    (==) _ _         = False

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"


ret1 = Red == Green

ret2 = show Blue

{-
The Read typeclass is essentially the opposite of Show.
It defines functions that wsill take
a String, parse it, and return data in any type that is a member of Read. 
The most useful function in Read is read.

You can define a simple
parser by providing an implementation for the readsPrec function. Your
implementation can return a list containing exactly one tuple on a successful
parse, or it can return an empty list on an unsuccessful parse.

Read is not widely used
While it is possible to build sophisticated parsers using the Read
typeclass, many people find it easier to do so using Parsec, and rely
on Read only for simpler tasks.
-}

instance Read Color where
    -- readsPrec is the main function for parsing input
    readsPrec _ value =
        -- We pass tryParse a list of pairs. Each pair has a string
        -- and the desired return value. tryParse will try to match
        -- the input to one of these strings.
        tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
        where tryParse [] = [] -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                  -- Compare the start of the string to be parsed to the
                  -- text we are looking for.
                  if (take (length attempt) value) == attempt
                      -- If we have a match, return the result and the
                      -- remaining input
                      then [(result, drop (length attempt) value)]
                      -- If we don't have a match, try the next pair
                      -- in the list of attempts.
                      else tryParse xs

ret3 = (read "Blue")::Color


{-
  runhaskell ./Exercise02.hs
-}
main :: IO ()
main = do
  putStrLn ""
  putStrLn "[Inicio]"

  putStrLn "[ret1]"
  putStrLn $ show ret1

  putStrLn "[ret2]"
  putStrLn ret2

  putStrLn "[ret3]"
  putStrLn $ show ret3

  putStrLn "[Fin]"
  putStrLn ""

  
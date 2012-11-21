module Main where

import Control.Arrow
import Data.Either

-- Fanout &&&
-- Send the input to both argument arrows and combine their output.
f1 :: String -> (String, String)
f1 x = ("A" ++) &&& ("B" ++) $ x

-- Split ***
-- Split the input between the two argument arrows and combine their output.
f2 :: String -> (String, String)
f2 x = ("A" ++) *** ("B" ++) $ (x, x)

-- Left-to-right composition >>>
f3 :: String -> String
f3 x = ("A" ++) >>> ("B" ++) $ x

-- first
-- Send the first component of the input through the argument arrow, and copy 
-- the rest unchanged to the output.
f4 :: String -> (String, String)
f4 x = ("A" ++) *** ("B" ++) >>> first ("C" ++) $ (x, x)

-- second
-- Send the second component of the input through the argument arrow, and copy 
-- the rest unchanged to the output.
f5 :: String -> (String, String)
f5 x = ("A" ++) *** ("B" ++) >>> second ("C" ++) $ (x, x)

-- Choice +++
-- Split the input between the two argument arrows, retagging and 
-- merging their outputs.
f6 :: String -> Either String String
f6 x = ("A" ++) +++ ("B" ++) $ (Right x)

-- Fainin |||
-- Split the input between the two argument arrows and merge their outputs.
f7 :: String -> String
f7 x = ("A" ++) ||| ("B" ++) $ (Right x)

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start."
    putStrLn ""

    putStr "Fanout &&&       "
    putStrLn (show (f1 "x"))
    putStrLn ""

    putStr "Split ***        "
    putStrLn (show (f2 "x"))
    putStrLn ""

    putStr "L-to-R comp >>>  "
    putStrLn (show (f3 "x"))
    putStrLn ""

    putStr "First            "
    putStrLn (show (f4 "x"))
    putStrLn ""

    putStr "Second           "
    putStrLn (show (f5 "x"))
    putStrLn ""

    putStr "Choice +++       "
    putStrLn (show (f6 "x"))
    putStrLn ""

    putStr "Fanin |||        "
    putStrLn (show (f7 "x"))
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
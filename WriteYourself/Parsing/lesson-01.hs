{- lesson-01.hs

> ./lesson-01.sh
> ./lesson-01 "+"

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

http://legacy.cs.uu.nl/daan/download/parsec/parsec.html

-}
module Main where
import System.Environment
import Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

blanks :: Parser ()
blanks = skipMany1 space

{-
Following typical Haskell convention, Parsec returns an Either data type, 
using the Left constructor to indicate an error and the Right one 
for a normal value.
-}

readExpr :: String -> String
readExpr input = case parse (blanks >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr $ getArg args 0) where
        getArg args n = args !! n

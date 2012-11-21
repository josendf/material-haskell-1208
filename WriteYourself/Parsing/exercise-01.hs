{- exercise-01.hs

> ./exercise-01.sh
> ./exercise-01 "123"

Exercise
========
Rewrite parseNumber using
 - 1 do-notation
 - 2 explicit sequencing with the >>= operator

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

http://legacy.cs.uu.nl/daan/download/parsec/parsec.html
-}
module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

blanks :: Parser ()
blanks = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom


{- 

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

Excercise
=========
Rewrite parseNumber using
 - 1 do-notation
 - 2 explicit sequencing with the >>= operator
-}

readDgts :: [Char] -> LispVal
readDgts dgts = Number (read dgts)

parseNumberDo :: Parser LispVal
parseNumberDo = do
                dgts <- many1 digit
                return (readDgts dgts)

parseNumberSq :: Parser LispVal
parseNumberSq = (many1 digit) >>= return . readDgts

parseNumber = parseNumberSq

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"
    
main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr $ getArg args 0) where
        getArg args n = args !! n

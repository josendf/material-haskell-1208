{- exercise-02.hs

Exercise
========
Our strings aren't quite R5RS compliant, because they don't support escaping 
of internal quotes within the string.

Change parseString so that \" gives a literal quote character instead of
terminating the string.

You may want to replace noneOf "\"" with a new parser action that accepts
either a non-quote character or a backslash followed by a quote mark.

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

http://legacy.cs.uu.nl/daan/download/parsec/parsec.html
-}
module Main where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec


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


{-
Exercise
========
Change parseString so that \" gives a literal quote character instead of
terminating the string.

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

-}


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (strLetter)
                 char '"'
                 return $ String x

strLetter :: Parser Char
strLetter = do { c <- noneOf "\"\\"; return c }
          <|> strEscape

strEscape :: Parser Char
strEscape  = char '\\'

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber = liftM (Number . read) $ many1 digit

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

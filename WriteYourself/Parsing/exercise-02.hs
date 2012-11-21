{- exercise-02.hs

> ./exercise-02.sh
> ./exercise-02 123
> ./exercise-02 \"abc\"
> ./exercise-02 "\"abc \\\"def\\\" ghi\""

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

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


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
strLetter = strText
          <|> strEscape

strText :: Parser Char
strText  = do c <- noneOf "\"\\"
              return c

strEscape :: Parser Char
strEscape  = do char '\\'
                escapedChar

escapedChar :: Parser Char
escapedChar  = do c <- oneOf "\"rnt"
                  return $ case c of
                            'r' -> '\r'
                            'n' -> '\n'
                            't' -> '\t'
                            _   -> c

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
    Right val -> "Found: " ++ show val
    
main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr $ getArg args 0) where
        getArg args n = args !! n

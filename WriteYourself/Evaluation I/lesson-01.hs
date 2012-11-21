{- lesson-01.hs

> ./lesson-01.sh
> ./lesson-01 "(a (dotted . list) test)"
> ./lesson-01 "(a '(quoted (dotted . list)) test)"

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Evaluation,_Part_1

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

{-
Let's start by telling Haskell how to print out a string representation of 
the various possible LispVals.

This is our first real introduction to pattern matching.

Pattern matching is a way of destructuring an algebraic data type,
selecting a code clause based on its constructor and then binding the 
components to variables.

Any constructor can appear in a pattern; that 
pattern matches a value if the tag is the same as the value's tag 
and all subpatterns match their corresponding components.

Patterns can be nested arbitrarily deep, with matching proceeding in 
an inside -> outside, left -> right order.

The clauses of a function 
definition are tried in textual order, until one of the patterns matches. 
-}

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ 
                                unwordsList head ++ " . " ++ 
                                showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal



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


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr blanks

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> blanks >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found: " ++ show val
    
main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr $ getArg args 0) where
        getArg args n = args !! n

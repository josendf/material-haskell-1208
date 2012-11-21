{- lesson-02.hs

> ./lesson-02.sh
> ./lesson-02 2
> ./lesson-02 "\"a string\""

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
Now, we start with the beginnings of an evaluator. The purpose of an 
evaluator is to map some "code" data type into some "data" data type, the 
result of the evaluation. In Lisp, the data types for both code and data are 
the same, so our evaluator will return a LispVal.

Other languages often have more complicated code structures, with a variety 
of syntactic forms.

Evaluating numbers, strings, booleans, and quoted lists is fairly simple: 
return the datum itself.

This introduces a new type of pattern.
The notation val@(String _) matches against any LispVal that's a 
string and then binds val to the whole LispVal, and not just the 
contents of the String constructor.

The result has type LispVal instead of type String. 
-}

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val


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

{-
Let's integrate eval into our existing code.
Start by changing readExpr back so it returns the
expression instead of a string representation of the expression:
-}

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

{-
And then change our main function to read an expression, evaluate it, 
convert it to a string, and print it out.
Now that we know about the >>= monad sequencing operator and the function 
composition operator, let's use them to make this a bit more concise:
-}
    
main :: IO ()
main = getArgs >>= print . eval . readExpr . head

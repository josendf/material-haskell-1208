{- lesson-03.hs

> ./lesson-03.sh
> ./lesson-03 "(+ 2 2)"
> ./lesson-03 "(+ 2 (-4 1))"
> ./lesson-03 "(- (+ 4 6 3) 3 5 2)"

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



eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

{- 
Next, we'll improve our Scheme so we can use it as a simple calculator. 

The built-in function lookup looks up a key (its first argument) in a 
list of pairs.

However, lookup will fail if no pair in the list contains the matching key.

To express this, it returns an instance of the built-in type Maybe.

We use the function maybe to specify what to do in case of either 
success or failure.

If the function isn't found, we return a Bool False value, 
equivalent to #f (we'll add more robust error-checking later). 
If it is found, we apply it to the arguments using ($ args), an operator 
section of the function application operator.
-}

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

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

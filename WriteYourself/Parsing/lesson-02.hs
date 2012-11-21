{- lesson-02.hs

> ./lesson-02.sh
> ./lesson-02 123

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

http://legacy.cs.uu.nl/daan/download/parsec/parsec.html

-}
module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad

{-
This is an example of an algebraic data type: it defines a set of possible 
values that a variable of type LispVal can hold.

Each alternative (called a constructor and separated by |) contains a tag for 
the constructor along with the type of data that that constructor can hold.

In this example, a LispVal can be:

+ An Atom, which stores a String naming the atom

+ A List, which stores a list of other LispVals.
    Haskell lists are denoted by brackets; also called a proper list

+ A DottedList, representing the Scheme form (a b . c)
    This stores a list of all elements but the last, and then stores the last 
    element as another field also called an improper list. 

+ A Number, containing a Haskell Integer

+ A String, containing a Haskell String

+ A Bool, containing a Haskell boolean value

Constructors and types have different namespaces, so you can have both a 
constructor named String and a type named String. Both types and constructor 
tags always begin with capital letters.

-}

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
A string is a double quote mark, followed by any number of non-quote 
characters, followed by a closing quote mark.

Once we've finished the parse and have the Haskell String returned from many, we 
apply the String constructor (from our LispVal data type) to turn it into a 
LispVal. Every constructor in an algebraic data type also acts like a function 
that turns its arguments into a value of its type. It also serves as a pattern 
that can be used in the left-hand side of a pattern-matching expression; we saw 
an example of this in Lesson 3.1 when we matched our parser result against the 
two constructors in the Either data type.

We then apply the built-in function return to lift our LispVal into the Parser 
monad. Remember, each line of a do-block must have the same type, but the result 
of our String constructor is just a plain old LispVal. Return lets us wrap that 
up in a Parser action that consumes no input but returns it as the inner value. 
Thus, the whole parseString action will have type Parser LispVal.

-}

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

{-
An atom is a letter or symbol, followed by any number of letters, digits, or 
symbols.

Here, we introduce another Parsec combinator, the choice operator <|>. This 
tries the first parser, then if it fails, tries the second. If either succeeds, 
then it returns the value returned by that parser. The first parser must fail 
before it consumes any input: we'll see later how to implement backtracking.

-}

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom


{- 
Finally, we create one more parser, for numbers. This shows one more way of 
dealing with monadic values.

It's easiest to read this backwards, since both function application ($) and 
function composition (.) associate to the right. The parsec combinator many1 
matches one or more of its argument, so here we're matching one or more digits. 
We'd like to construct a number LispVal from the resulting string, but we have a 
few type mismatches. First, we use the built-in function read to convert that 
string into a number. Then we pass the result to Number to get a LispVal. The 
function composition operator . creates a function that applies its right 
argument and then passes the result to the left argument, so we use that to 
combine the two function applications.

Unfortunately, the result of many1 digit is actually a Parser String, so our 
combined Number . read still can't operate on it. We need a way to tell it to 
just operate on the value inside the monad, giving us back a Parser LispVal. The 
standard function liftM does exactly that, so we apply liftM to our Number . 
read function, and then apply the result of that to our parser.

liftM :: Monad m => (a1 -> r) -> m a1 -> m rSource

Promote a function to a monad.

liftM f  =  \a -> do { a' <- a; return (f a') }

liftM f m lets a non-monadic function f operate on the contents of monad m

> liftM sin (Just 0)
Just 0.0

-}

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

{-
Let's create a parser that accepts either a string, a number, or an atom.
-}

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

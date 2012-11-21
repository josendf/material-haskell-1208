{-
  Chapter 3 Types
  Data Keyword
  Real World Haskell
-}
module Main where

{-

BookInfo after the data keyword is the name of our new type.

We call BookInfo a type constructor.

Once we define a type, we will use its type constructor to refer to it.

As we've already mentioned, a type name, and hence a type constructor, must 
start with a capital letter.

The Book that follows is the name of the value constructor 
, sometimes called a data constructor.

We use this to create a value of the BookInfo type. 

A value constructor's name must also start with a capital letter.

After Book, the Int, String, and [String] that follow are the 
components of the type.

-}
data BookInfo = Book Int String [String]
                deriving (Show)


ret1 = Book 9780135072455 "Algebra of Programming" 
        ["Richard Bird", "Oege de Moor"]

{--
We can introduce a synonym for an existing type at any time, in order to 
give a type a more descriptive name.

The type keyword introduces a type synonym.
The two names identify the same type, so type synonyms
are purely for making code more readable.
--}
type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

{--
An algebraic data type can have more than one value constructor.
Each value constructor is
separated in the definition by a | character.
--}
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)


{-
We can pattern match on an algebraic data type using its value constructors.

We can indicate that we don't care what is present in part of a pattern.

The notation for this is the underscore character (_), which we call 
a wild card.
-}
bookID      (Book id _     _) = id
bookTitle   (Book _  title _) = title
bookAuthors (Book _  _     authors) = authors

ret2 = "bookID: " ++ show (bookID ret1) ++ "\n" ++
       "bookTitle: " ++ show (bookTitle ret1)


{-
  runhaskell ./DataKeyword.hs
-}
main :: IO ()
main = do
  putStrLn ""
  putStrLn "[Inicio]"

  putStrLn "[ret1]"
  putStrLn $ show ret1

  putStrLn "[ret2]"
  putStrLn ret2

  putStrLn "[Fin]"
  putStrLn ""
{-
  putStrLn "<ret1>"
  putStrLn $ show ret1
  putStrLn "<ret2>"
  putStrLn $ show ret2
  putStrLn "<ret3>"
  putStrLn $ show ret3
-}

  
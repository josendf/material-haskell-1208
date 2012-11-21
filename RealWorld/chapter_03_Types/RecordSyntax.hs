{-
  Chapter 3 Types
  Record Syntax
  Real World Haskell
-}
module Main where


data BookInfo = Book Int String [String]
                deriving (Show)


ret1 = Book 9780135072455 "Algebra of Programming" 
        ["Richard Bird", "Oege de Moor"]


type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)


type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)


bookID      (Book id _     _) = id
bookTitle   (Book _  title _) = title
bookAuthors (Book _  _     authors) = authors

ret2 = "bookID: " ++ show (bookID ret1) ++ "\n" ++
       "bookTitle: " ++ show (bookTitle ret1)

{-
We can define a data type, and accessors for each of its components,
simultaneously.

For each of the fields that we name in our type definition, Haskell 
creates an accessor function of that name.

-}
data Customer = Customer {
                customerID :: CustomerID
              , customerName :: String
              , customerAddress :: Address
              } deriving (Show)

{-
We can still use the usual application syntax to create a value of this type
-}
ret3 = Customer 271828 "J.R. Hacker"
        ["255 Syntax Ct",
         "Milpitas, CA 95134",
         "USA"]

{-
Record syntax adds a more verbose optional notation for creating a value.
-}
ret4 = Customer {
        customerID = 271828
     , customerAddress = ["1048576 Disk Drive",
            "Milpitas, CA 95134",
            "USA"]
     , customerName = "Jane Q. Citizen"
     }

{-
  runhaskell ./RecordSyntax.hs
-}
main :: IO ()
main = do
  putStrLn ""
  putStrLn "[Inicio]"

  putStrLn "[ret1]"
  putStrLn $ show ret1

  putStrLn "[ret2]"
  putStrLn ret2

  putStrLn "[ret3]"
  putStrLn $ show ret3

  putStrLn "[ret4]"
  putStrLn $ show ret4

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

  
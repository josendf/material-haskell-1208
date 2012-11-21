{-# LANGUAGE  DeriveDataTypeable #-}
module Main where

import Data.Data
import Data.Typeable
import Data.Generics

{--
Scrap your boilerplate: generic programming in Haskell
http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/
--}


{--
Find all people in tree and increase their salary by 10%
--}

data Company    = C [Dept]                 deriving (Eq, Show, Typeable, Data)
data Dept       = D Name Manager [SubUnit] deriving (Eq, Show, Typeable, Data)
data SubUnit    = PU Employee | DU Dept    deriving (Eq, Show, Typeable, Data)
data Employee   = E Person Salary          deriving (Eq, Show, Typeable, Data)
data Person     = P Name Address           deriving (Eq, Show, Typeable, Data)
data Salary     = S Float                  deriving (Eq, Show, Typeable, Data)
type Manager    = Employee
type Name       = String
type Address    = String

company :: Company
company = C [

    D "Research" (E (P "Fred" "London") (S 10.0)) [
      PU (E (P "Ann" "London") (S 10.0))
      ]

  , D "Production" (E (P "Bill" "Glasgow") (S 15.0)) [
        DU (D "Development"   (E (P "Peter" "Glasgow") (S 8.0)) [])
      , DU (D "Manufacturing" (E (P "Mary" "Glasgow") (S 8.0)) [])
      ]
  ]

incSal :: Float -> Company -> Company
incSal k = everywhere (mkT (incS k))

incS :: Float -> Salary -> Salary
incS k (S s) = S (s * k)

main :: IO ()
main = do

    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    print company

    print $ incSal 1.2 company

    putStrLn ""
    putStrLn "Done."
    putStrLn ""

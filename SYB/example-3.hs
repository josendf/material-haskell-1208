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

{--
mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
Make a generic transformation; 
start from a type-specific case; preserve the term otherwise

everywhere :: (forall a. Data a => a -> a) -> forall a. Data a => a -> a
Apply a transformation everywhere in bottom-up manner   

--}

incSal :: Float -> Company -> Company
incSal k = everywhere (mkT (incS k))

incS :: Float -> Salary -> Salary
incS k (S s) = S (s * k)

{--
Add up the salaries of all the employees in the tree
--}
accSal :: Company -> Float
accSal = everything (+) (mkQ 0 getS)

getS :: Salary -> Float
getS (S x) = x

{--
Looking for one result
--}
findDept :: String -> Company -> Maybe Dept
findDept s = everything orElse (mkQ Nothing (findD s))

findD :: String -> Dept -> Maybe Dept
findD s d@(D s' _ _) = if s==s' then Just d else Nothing

main :: IO ()
main = do

    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    print company
    putStrLn ""

    print $ accSal company
    putStrLn ""

    let company' = incSal 1.2 company 
    print $ company'
    putStrLn ""

    print $ accSal company'
    putStrLn ""

    print $ findDept "Production" company'
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
    putStrLn ""

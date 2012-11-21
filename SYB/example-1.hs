module Main where

{--
Scrap your boilerplate: generic programming in Haskell
http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/
--}


{--
Find all people in tree and increase their salary by 10%
--}

data Company    = C [Dept]                 deriving Show
data Dept       = D Name Manager [SubUnit] deriving Show
data SubUnit    = PU Employee | DU Dept    deriving Show
data Employee   = E Person Salary          deriving Show
data Person     = P Name Address           deriving Show
data Salary     = S Float                  deriving Show
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

{-- The problem: boilerplate code --}
incSal :: Float -> Company -> Company
incSal k (C ds) = C (map (incD k) ds)

incD :: Float -> Dept -> Dept
incD k (D n m us) = D n (incE k m) (map (incU k) us)

incU :: Float -> SubUnit -> SubUnit
incU k (PU e) = PU (incE k e)
incU k (DU d) = DU (incD k d)

incE :: Float -> Employee -> Employee
incE k (E p s) = E p (incS k s)

incS :: Float -> Salary -> Salary
incS k (S f) = S (k*f)

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

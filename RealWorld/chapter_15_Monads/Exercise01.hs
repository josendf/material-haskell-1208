{-
  Chapter 15 Monads
  Real World Haskell
  runhaskell ./Exercise01.hs 
  start-process cmd /K,'runhaskell ./Exercise01.hs'
-}
module Main where
import System.Directory
import System.FilePath

import Control.Monad

data Context = Home | Mobile | Business
               deriving (Eq, Show)

type Phone = String


person1 = [(Home, "+355-652-55512")]

person2 = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
           (Home, "+47-925-55-121"),   (Business, "+47-922-25-551")]

person3 = [(Business, "+260-02-55-5121")]

contextIs a (b, _) = a == b

{-

Notice that these two functions structure their case expressions similarly: one 
alternative handles the case where the first lookup returns an empty result, 
while the other handles the non-empty case.

-}

onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps
                        Just n -> Just n

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
    where numbers = case filter (contextIs Business) ps of
                      [] -> filter (contextIs Mobile) ps
                      ns -> ns

ret1 = "person1: " ++ (show $ onePersonalPhone person1) ++ " " ++
       "person3: " ++ (show $ onePersonalPhone person3)

ret2 = "person2: " ++ (show $ allBusinessPhones person2)


{-

Haskell's Control.Monad module defines a typeclass, MonadPlus, that lets us 
abstract the common pattern out of our case expressions.

class Monad m => MonadPlus m where
   mzero :: m a	
   mplus :: m a -> m a -> m a

-}
btrAllBusinessPhones :: [(Context, Phone)] -> Maybe Phone
btrAllBusinessPhones ps = lookup Business ps `mplus` lookup Mobile ps


allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps

ret3 = "person1: " ++ (show $ allPersonalPhones person1) ++ " " ++
       "person3: " ++ (show $ allPersonalPhones person3)

ret4 = "person2: " ++ (show $ btrAllBusinessPhones person2)


main :: IO ()
main = do
    -- currDir <- getCurrentDirectory
    -- let root = currDir </> ".."

    putStrLn ""
    
    putStrLn ""
    putStrLn "[ret1]"
    putStrLn $ show ret1

    putStrLn ""
    putStrLn "[ret2]"
    putStrLn $ show ret2

    putStrLn ""
    putStrLn "[ret1]"
    putStrLn $ show ret3

    putStrLn ""
    putStrLn "[ret2]"
    putStrLn $ show ret4

    putStrLn ""


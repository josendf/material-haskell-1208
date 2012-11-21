{--
    The Send More Money Problem consists in finding distinct digits
    for the letters D, E, M, N, O, R, S  such that S and M are 
    different from zero (no leading zeros) and the equation
    
          SEND
        + MORE
        -------
         MONEY
    
    is satisfied.
    
    The unique solution of the problem is:
    
        9567 + 1085 = 10652
--}

module SendPlusMore where

import Control.Monad
import Control.Monad.State.Lazy
import Data.List

{--
    Monad.Reader 11
    Douglas M. Auclair: MonadPlus: What a Super Monad!
    http://www.haskell.org/wikiupload/6/6a/TMR-Issue11.pdf
--}

-- Reads from a list, choose an element and return it along with the 
-- rest of the list
splits :: Eq a => [a] -> [(a, [a])]
splits list = list >>= (\x -> return (x, delete x list))

-- We lift the splits computation into the State monad transformer
choose :: Eq a => StateT [a] [] a
choose = StateT (\s -> splits s)

-- converts a list of digits to the corresponding integer
num :: [Int] -> Int
num = foldl ((+) . (* 10)) 0

{--

This alternative representation uses the do-notation,
with constraints defined by guards.

A guard is of the following form:
guard :: MonadPlus m -> Bool -> m ()

What does that do for us? Recall that MonadPlus types have a base value
(mzero) representing failure.

Now guard translates the input Boolean constraint into either mzero (failure) 
or into a success value.

Since the entire monadic computation is chained by mplus, a failure of one 
test voids that entire branch (because the failure propogates through the 
entire branch of computation).
--}

sendmory :: StateT [Int] [] [Int]
sendmory = do 
              let m = 1
              let o = 0
              -- s it is either 9 if there’s no carry from addition of
              -- the other digits or 8 if there is
              s <- choose
              guard (s > 7)
              e <- choose
              d <- choose
              y <- choose
              n <- choose
              r <- choose
              guard (num [s, e, n, d] + num [m, o, r , e] == num [m, o, n, e, y])
              return [s, e, n, d, m, o, r, y]

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start"
    putStrLn ""

    -- As we’ve grounded M and O to 1 and 0 respectively, we’ve eliminated
    -- those options from the digit list.
    let digits = [2..9]
    let r = evalStateT sendmory digits
    print r


    putStrLn "Done."
    
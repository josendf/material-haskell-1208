module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either

loadText :: String -> IO [String]
loadText path = pure lines <*> readFile path

countWordA w = Kleisli readFile 
                >>> arr words
                >>> arr (filter (== w))
                >>> arr length
                >>> Kleisli print

countWordM w = (>>= print)
                . liftM (length . filter (==w) . words)
                . readFile

testM :: Monad m => (a -> m b) -> (c -> m d) -> (a, c) -> m (b, d)
testM f g (a, c) = do
    b <- f a
    d <- g c
    return (b, d)

testAp :: Applicative f => (a -> f b) -> (c -> f d) -> (a, c) -> f (b, d)
testAp f g = uncurry (liftA2 (,)) . (f *** g)

testAr :: Monad m => (a -> m b) -> (c -> m d) -> (a, c) -> m (b, d)
testAr f g = runKleisli $ Kleisli f *** Kleisli g

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start."
    putStrLn ""

    text <- loadText "Exercise1.hs"

    putStrLn ""
    putStrLn $ show text
    putStrLn ""

    putStrLn ""
    putStrLn "Arrow"
    runKleisli (countWordA "the") "Exercise1.hs"
    putStrLn ""

    putStrLn ""
    putStrLn "Monad"
    (countWordM "the") "Exercise1.hs"
    putStrLn ""


    putStrLn ""
    putStrLn "Done."
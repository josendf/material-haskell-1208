{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

{-- Dependency Injection Haskell Style
http://mikehadlow.blogspot.com/2011/05/dependency-injection-haskell-style.html
--}

{-- First let’s define some services:
--}

data Report = Report Int String
            deriving Show

-- this could talk to a database
type GetReport = Int -> IO Report

-- this could talk to an email API
type SendReport = Report -> IO ()

-- this takes a report id and does something with it
type ProcessReport = Int -> IO ()

{-- Now let’s define some implementations:
--}

-- getReport simply creates a new report with the given id
getReport :: GetReport
getReport id = return $ Report id "Hello"

-- sendReport simply prints the report
sendReport :: SendReport
sendReport report = putStrLn $ "Sent: " ++ show report

-- processReport uses a GetReport and a SendReport to process a report
processReport :: GetReport -> SendReport -> ProcessReport
processReport get send id = do
  r <- get id
  send r

class Resolve a where resolve :: a

instance Resolve GetReport where
    resolve = getReport

instance Resolve SendReport where
    resolve = sendReport

instance Resolve ProcessReport where
    resolve = processReport resolve resolve

{-- Partial function application is equivalent to dependency injection in OO.
--}

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Start..."
  putStrLn ""

  {-- Note that we partially apply processReport with two resolve calls
      that will provide implementations of the service types. --}

  let proc = resolve :: ProcessReport
  proc 123


  putStrLn ""
  putStrLn "Done."
  putStrLn ""

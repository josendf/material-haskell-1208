{-
  Viterbi algorithm examples
  See http://en.wikipedia.org/wiki/Viterbi_algorithm

  Using the hmm package
  See http://hackage.haskell.org/package/hmm
      http://izbicki.me/blog/using-hmms-in-haskell-for-bioinformatics
-}
module Main where
import qualified Data.Array           as A
import           Data.HMM

{-
        TYPES
-}

{-
  See http://hackage.haskell.org/package/logfloat
  This module presents a type for storing numbers in the log-domain.
  The main reason for doing this is to prevent underflow when multiplying
  many small probabilities as is done in Hidden Markov Models and other
  statistical models often used for natural language processing.
-}

data State              = Healthy | Fever
                          deriving (Eq, Ord, Enum, Bounded, Show)

data Observation        = Normal | Cold | Dizzy
                          deriving (Eq, Ord, Enum, Bounded, Show)

{-
        HELPERS
-}
allStates :: [State]
allStates = [(minBound :: State) ..]

allObs :: [Observation]
allObs = [(minBound :: Observation) ..]

{-
        MAIN
-}

main :: IO ()
main = do
  putStrLn "Start..."

  let tdata = [Normal 
              ,Normal 
              ,Normal 
              ,Normal 
              ,Normal 
              ,Normal 
              ,Normal 
              ,Normal 
              ,Cold  
              ,Dizzy
              ,Normal 
              ,Normal 
              ,Normal 
              ,Normal 
              ]

  let tdata' = A.listArray (1, length tdata) tdata

  let hmm = simpleHMM allStates allObs

{-
  The Baum–Welch algorithm is used to find the unknown
  parameters the hidden Markov model (HMM).
  See http://en.wikipedia.org/wiki/Baum%E2%80%93Welch_algorithm
-}
  let hmm' = baumWelch hmm  tdata' 3

{-
  The patient visits three days in a row and the doctor discovers that on the
  first day she feels normal, on the second day she feels cold, on the third day
  she feels dizzy. The doctor has a question: what is the most likely sequence of
  health condition of the patient that would explain these observations?
  This is answered by the Viterbi algorithm.
-}
  let obs = [Normal, Cold, Dizzy]

  let result = viterbi hmm' (A.listArray (1, length obs) obs)

  putStrLn ""
  print result
  putStrLn ""

  saveHMM "hmm-trained.txt" hmm'

  putStrLn "Done."

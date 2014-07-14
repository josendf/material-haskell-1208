{-
  Viterbi algorithm examples
  See http://en.wikipedia.org/wiki/Viterbi_algorithm

  Using the hmm package
  See http://hackage.haskell.org/package/hmm
      http://izbicki.me/blog/using-hmms-in-haskell-for-bioinformatics
-}
module Main where
import qualified Data.Map.Strict      as M
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

{-
 Start Probability represents the doctor's belief about which state the
 Hidden Markov Model is in when the patient first visits (all he knows
 is that the patient tends to be healthy).
-}
  let startProbs = M.fromList [(Healthy, 0.6), (Fever, 0.4)]

{-
  The Transition Probability represents the change of the health condition
  in the underlying Markov chain. In this example, there is only a 30% chance
  that tomorrow the patient will have a fever if he is healthy today. 
-}
  let transitionProbs = M.fromList [
        (Healthy, M.fromList [(Healthy, 0.7), (Fever, 0.3)]),
        (Fever,   M.fromList [(Healthy, 0.4), (Fever, 0.6)])]

{-
  The Emission Probability represents how likely the patient is to feel
  on each day. If he is healthy, there is a 50% chance that he feels normal;
  if he has a fever, there is a 60% chance that he feels dizzy.
-}
  let emissionProbs = M.fromList [
        (Healthy, M.fromList [(Normal, 0.5),
                              (Cold,   0.4),
                              (Dizzy,  0.1)]),
        (Fever,   M.fromList [(Normal, 0.1),
                              (Cold,   0.3),
                              (Dizzy,  0.6)])]

  let hmm = HMM { states      = allStates
                , events      = allObs
                , initProbs   = \s -> startProbs M.! s
                , transMatrix = \s1 s2 -> (transitionProbs M.! s1) M.! s2
                , outMatrix   = \s e -> (emissionProbs M.! s) M.! e }


{-
  The patient visits three days in a row and the doctor discovers that on the
  first day she feels normal, on the second day she feels cold, on the third day
  she feels dizzy. The doctor has a question: what is the most likely sequence of
  health condition of the patient that would explain these observations?
  This is answered by the Viterbi algorithm.
-}

  let obs = [Normal, Cold, Dizzy]

  let result = viterbi hmm (A.listArray (1, length obs) obs)

  putStrLn ""
  print result
  putStrLn ""

  putStrLn "Done."

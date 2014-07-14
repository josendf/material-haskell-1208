{-
  Viterbi algorithm examples
  See http://en.wikipedia.org/wiki/Viterbi_algorithm
-}
module Main where
import           Data.List (maximumBy)
import qualified Data.Map.Strict      as M
import qualified Data.Number.LogFloat as LF

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
type Probability        = LF.LogFloat 

data State              = Healthy | Fever
                          deriving (Eq, Ord, Enum, Bounded, Show)

data Observation        = Normal | Cold | Dizzy
                          deriving (Eq, Ord, Enum, Bounded, Show)

newtype StateProb       = StateProb (M.Map State Probability)
                          deriving (Show)

newtype TransitionProb  = TransitionProb (M.Map State StateProb)
                          deriving (Show)

newtype ObservationProb = ObservationProb (M.Map Observation Probability)

newtype EmissionProb    = EmissionProb (M.Map State ObservationProb)

newtype Step            = Step (M.Map Int StateProb)
                          deriving (Show)

newtype Path            = Path (M.Map State [State])
                          deriving (Show)

newtype V               = V (Step, Path)
                          deriving (Show)
    
newtype VResult         = VResult (State, Probability, [State])
                          deriving (Show)

{-
        HELPERS
-}

allStates :: [State]
allStates = [(minBound :: State) ..]

stepProb :: Step -> Int -> StateProb
stepProb (Step x) y = x M.! y

stateProb :: StateProb -> State -> Probability
stateProb (StateProb s) y = s M.! y

emitProb :: EmissionProb -> State -> Observation -> Probability
emitProb (EmissionProb e) y z =
  let (ObservationProb o) = e M.! y in o M.! z

transProb :: TransitionProb -> State -> State -> Probability
transProb (TransitionProb t) y z =
  let (StateProb s) = t M.! y in s M.! z

initV :: [Observation]
      -> [State]
      -> StateProb
      -> EmissionProb
      -> V
initV obs states starts emits =
  let steps  = Step (M.singleton 0 (StateProb (M.fromList pairs)))
      pairs  = map (\s -> (s, stateProb starts s * emitProb emits s obs0)) states
      obs0   = head obs
      paths  = Path (M.fromList (map (\s -> (s, [s])) states))
  in V (steps, paths)

updateStep :: Step
           -> Int
           -> State
           -> Probability
           -> Step
updateStep (Step step) t s p =
  case M.lookup t step of
    Nothing             -> Step (M.insert t (StateProb (M.singleton s p)) step)
    Just (StateProb sp) -> Step (M.insert t (StateProb (M.insert s p sp)) step)

appendPath :: Path
           -> Path
           -> State
           -> State
           -> Path
appendPath (Path path) (Path current) k s = 
  case M.lookup k current of
    Nothing -> Path (M.insert s [s] path)
    Just ss -> Path (M.insert s (ss ++ [s]) path)

compSnd :: (a, Probability)
        -> (a, Probability)
        -> Ordering
compSnd (_, x) (_, y) = compare x y

computeStep :: [State]
            -> TransitionProb
            -> EmissionProb
            -> Path
            -> V
            -> Int
            -> State
            -> V
computeStep states trans emits p0 (V (steps, paths)) t s = 
  let t'       = toEnum t :: Observation
      pairs    = map (\sx -> (sx, stateProb stp sx *
                                  transProb trans sx s *
                                  emitProb emits s t')) states
      (ms, mp) = maximumBy compSnd pairs
      step'    = updateStep steps t s mp
      path'    = appendPath paths p0 ms s
      stp      = stepProb steps (t - 1)
  in V (step', path')

computeResult :: [State] -> V -> VResult
computeResult states (V (Step steps, Path paths)) =
  let nums     = M.size steps
      n        = if nums < 2 then 0 else nums - 1 
      st       = steps M.! n
      pairs    = map (\s -> (s, stateProb st s)) states
      (ms, mp) = maximumBy compSnd pairs
  in VResult(ms, mp, paths M.! ms)

{-
        VITERBI
-}

viterbi :: [Observation]
        -> [State]
        -> StateProb
        -> TransitionProb
        -> EmissionProb
        -> VResult
viterbi observations states starts trans emits =

  let (V (steps, paths)) = initV observations states starts emits 
      numo               = length observations
      obs                = if numo < 2 then [] else [1..numo - 1]
      v0                 = V (steps, Path M.empty)
  in computeResult states (go obs states paths v0)
     
  where
    go :: [Int] -> [State] -> Path -> V -> V
    go obs sts path0 v@(V (steps, paths))
      | null obs  = V (steps, path0)
      | null sts  = go (tail obs) states paths (V (steps, Path M.empty))
      | otherwise = let v' = computeStep states trans emits path0 v (head obs) (head sts)
                    in  go obs (tail sts) path0 v'

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
  let startProbs = StateProb $ M.fromList [(Healthy, 0.6), (Fever, 0.4)]

{-
  The Transition Probability represents the change of the health condition
  in the underlying Markov chain. In this example, there is only a 30% chance
  that tomorrow the patient will have a fever if he is healthy today. 
-}
  let transitionProbs = TransitionProb $ M.fromList [
        (Healthy, StateProb $ M.fromList [(Healthy, 0.7), (Fever, 0.3)]),
        (Fever,   StateProb $ M.fromList [(Healthy, 0.4), (Fever, 0.6)])]

{-
  The Emission Probability represents how likely the patient is to feel
  on each day. If he is healthy, there is a 50% chance that he feels normal;
  if he has a fever, there is a 60% chance that he feels dizzy.
-}
  let emissionProbs = EmissionProb $ M.fromList [
        (Healthy, ObservationProb $ M.fromList [(Normal, 0.5),
                                                (Cold,   0.4),
                                                (Dizzy,  0.1)]),
        (Fever,   ObservationProb $ M.fromList [(Normal, 0.1),
                                                (Cold,   0.3),
                                                (Dizzy,  0.6)])]

{-
  The patient visits three days in a row and the doctor discovers that on the
  first day she feels normal, on the second day she feels cold, on the third day
  she feels dizzy. The doctor has a question: what is the most likely sequence of
  health condition of the patient that would explain these observations?
  This is answered by the Viterbi algorithm.
-}
  let obs = [Normal, Cold, Dizzy]
  let result = viterbi obs allStates startProbs transitionProbs emissionProbs

  putStrLn ""
  print result
  putStrLn ""

  putStrLn "Done."

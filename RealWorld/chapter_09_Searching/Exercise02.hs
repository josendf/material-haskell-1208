{-
  Chapter 9 IO
  Real World Haskell
  runhaskell ./Exercise02.hs 
  
  ghc -O2 -prof -auto-all -caf-all -rtsopts -fforce-recomp -rtsopts --make Exercise02.hs
  
  Powershell:
  & ghc -O2 -prof -auto-all -caf-all -fforce-recomp -rtsopts --make Exercise02.hs
  
  we can have the compiler insert the cost centres on all top level functions 
  for us by compiling with the -auto-all flag
 
  -fforce-recomp flag to to force full recompilation
  
  To get accurate numbers for these values, known as "constant applicative 
  forms", or CAFs, we use the -caf-all flag
  
  ./Exercise02 +RTS -sstderr
  
  
   Powershell: & ./Exercise02 +RTS -sstderr
 
  
  Collecting runtime statistics. To get access to that kind of information, 
  GHC lets us pass flags directly to the Haskell runtime, using the special 
  +RTS and -RTS flags to delimit arguments reserved for the runtime system. 
  The application itself won't see those flags, as they're immediately 
  consumed by the Haskell runtime system.
  
  In particular, we can ask the runtime system to gather memory and 
  garbage collector performance numbers with the -s flag (as well as control 
  the number of OS threads with -N, or tweak the stack and heap sizes). 

    We can now run this annotated program with time profiling enabled 

  ./Exercise02 +RTS -sstderr -p
    
  $ ./Exercise02 +RTS -sstderr -p

  The runtime will dump its profiling information into a file, *.prof

-}
module Main where
import System.Directory (
      getCurrentDirectory
    )
import System.FilePath (
      (</>)
    , takeExtension
    )
import Directory (
      getRecursiveContents
    )

simpleFind :: (FilePath -> Bool) -> FilePath -> IO ([FilePath], [FilePath])
simpleFind prd path = do
    names <- getRecursiveContents path
    return (names, (filter prd names))

showSmry :: ([FilePath],[FilePath]) -> String
showSmry (found,scanned) = "Found (" ++ showFound ++ 
    ") matches in (" ++ showScanned ++ ") files."
    where
        showFound   = show (length found)
        showScanned = show (length scanned)

{-
mapM_
Map each element of a structure to a monadic action, evaluate these actions 
from left to right, and ignore the results.
-}

putMatches :: ([FilePath],[FilePath]) -> IO ()
putMatches (found,_) = mapM_ (putStrLn . show) found

main :: IO ()
main = do
    currDir <- getCurrentDirectory

    let root = currDir </> ".."

    let filt x ext = (takeExtension x) == ext

    rslt <- simpleFind (filt ".hs") root 

    putMatches rslt

    putStrLn (showSmry rslt) 


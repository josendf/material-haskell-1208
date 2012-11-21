{-
  Chapter 9 IO
  Real World Haskell
  runhaskell ./Exercise05.hs 
-}
module Main where

import System.Directory (getCurrentDirectory)
import Control.Monad (liftM)
import Data.Char (toLower)
import System.FilePath (
      (</>)
    , takeFileName
    , takeExtension
    , takeBaseName
    )

import Traverse (Info(..), getInfo, getUsefulContents, isDirectory)

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
        endSeed <- fold initSeed path
        return (unwrap endSeed)

    where
        fold seed subpath = getUsefulContents subpath >>= walk subpath seed

        walk cPath seed (name:names) = do
            let path1 = cPath </> name
            info <- getInfo path1
            case iter seed info of
                done@(Done _) -> return done
                Skip seed1    -> walk cPath seed1 names
                Continue seed1
                    | isDirectory info -> do
                        next <- fold seed1 path1
                        case next of
                            done@(Done _) -> return done
                            seed2         -> walk cPath (unwrap seed2) names
                    | otherwise -> walk cPath seed1 names
        walk _ seed _ = return (Continue seed)


countDirectories count info =
    Continue (if isDirectory info
              then count + 1
              else count)

atMostThreeHs :: Iterator [FilePath]
atMostThreeHs paths info
    | length paths == 3
      = Done paths
    | isDirectory info && takeFileName path `elem` [".svn", ".git"]
      = Skip paths
    | extension `elem` [".hs", ".lhs"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info


main :: IO ()
main = do
    putStrLn "" 

    currDir <- getCurrentDirectory

    let root = currDir </> ".."

    dirCount <- foldTree countDirectories 0 root
    putStrLn "[dirCount]" 
    putStrLn (show dirCount) 

    threeHs <- foldTree atMostThreeHs [] root
    putStrLn "[threeHs]" 
    putStrLn (show threeHs) 


    return ()


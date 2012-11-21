{-
  Chapter 9 IO
  Real World Haskell
  runhaskell ./Exercise01.hs 
-}
module Main where
import System.Directory (
      getCurrentDirectory
    )
import System.FilePath (
      (</>)
    )
import Directory (
      getRecursiveContents
    )

main :: IO ()
main = do
    currDir <- getCurrentDirectory

    let root = currDir </> ".."

    paths <- getRecursiveContents root

    mapM (putStrLn . show) paths

    let smry = show (length paths)

    putStrLn $ "Found (" ++  smry ++ ") files."


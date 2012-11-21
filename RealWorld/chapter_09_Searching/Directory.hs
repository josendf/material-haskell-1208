{-
  Chapter 9 IO
  Real World Haskell
-}
module Directory (
      getRecursiveContents
    ) where

import Control.Monad (
    forM
    )
import System.Directory (
      doesDirectoryExist 
    , getDirectoryContents
    )
import System.FilePath (
      (</>)
    )

{-
  forM is mapM with its arguments flipped:
  mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
  forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
  
  The stylistic rule of thumb to follow here is to use whichever of mapM or 
  forM lets you write the tidiest code.
  If the loop body and the expression computing the data over which
  you're looping are both short, it doesn't matter which you use.
  If the loop is short, but the data is long, use mapM. If the loop is 
  long, but the data short, use forM.
  And if both are long, use a let or where clause to make one of them short.
-}

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

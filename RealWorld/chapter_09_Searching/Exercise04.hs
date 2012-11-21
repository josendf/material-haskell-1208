{-
  Chapter 9 IO
  Real World Haskell
  runhaskell ./Exercise04.hs 
-}
module Main where
import Control.Monad (
      filterM
    )
import System.Directory (
      getCurrentDirectory
    , Permissions(..)
    , getModificationTime
    , getPermissions
    )
import System.FilePath (
      (</>)
    , takeExtension
    )
import Control.Exception (
      bracket 
    , handle
    , IOException
    )
import System.IO (
      IOMode(..)
    , hClose
    , hFileSize
    , openFile
    )
import System.Time (
      ClockTime(..)
    )
import Directory (
      getRecursiveContents
    )

type InfoP a =  FilePath        -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> ClockTime       -- last modified
             -> a

type Predicate =  InfoP Bool

betterFind :: Predicate -> FilePath -> IO ([FilePath],[FilePath])
betterFind prd path = do

    found <- getRecursiveContents path

    matches <- filterM check found

    return (matches,found)

    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (prd name perms size modified)

exHandler :: IOException -> IO (Maybe Integer)
exHandler _ = return Nothing

{-
The bracket function takes three actions as arguments.
The first action acquires a resource. The second releases the resource.
The third runs in between, while the resource is acquired; let's call this
the “use” action. If the “acquire” action succeeds, the “release” action is
always called. This guarantees that the resource will always be released.
The “use” and “release” actions are each passed the resource acquired by
the “acquire” action.
-}
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle exHandler $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` constP k w x y z

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP
infix 4 ==?

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP
infixr 3 &&?

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP
infix 4 >?

showSmry :: ([FilePath],[FilePath]) -> String
showSmry (matches,found) = "Found (" ++ showMatches ++ 
    ") matches in (" ++ showFound ++ ") files."
    where
        showMatches   = show (length matches)
        showFound = show (length found)

{-
mapM_
Map each element of a structure to a monadic action, evaluate these actions 
from left to right, and ignore the results.
-}

putMatches :: ([FilePath],[FilePath]) -> IO ()
putMatches (matches,_) = mapM_ (putStrLn . show) matches

main :: IO ()
main = do
    currDir <- getCurrentDirectory

    let root = currDir </> ".."

    let filt = liftPath takeExtension ==? ".hs" &&? sizeP >? 1

    rslt <- betterFind filt root 

    putMatches rslt

    putStrLn (showSmry rslt) 



module Traverse
    (
      Info(..)
    , getInfo
    , getUsefulContents
    , isDirectory
    ) where

import Control.Monad (filterM, forM, liftM)
import Data.List (partition)
import Data.Maybe (isJust)
import System.Directory (Permissions(..), getDirectoryContents,
                         getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension, (</>))
import Control.Exception (
      bracket 
    , handle
    , IOException
    )
import System.IO (IOMode(..), hClose, hFileSize, openFile)

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe ClockTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info

exHandler :: IOException -> IO (Maybe a)
exHandler _ = return Nothing

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle exHandler (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms


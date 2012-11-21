-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

module Helpers (
    timeit
  , getURL
) where

import Data.Time
import Network.HTTP
import Network.Browser
import Network.URI
import Data.ByteString (ByteString)

timeit :: IO a -> IO (a, Double)
timeit io = do
     t0 <- getCurrentTime
     a <- io
     t1 <- getCurrentTime
     return (a, realToFrac (t1 `diffUTCTime` t0))

getURL :: String -> IO ByteString
getURL url = do
  Network.Browser.browse $ do
    setCheckForProxy True
    setDebugLog Nothing
    setOutHandler (const (return ()))
    (_, rsp) <- request (getRequest' (escapeURIString isUnescapedInURI url))
    return (rspBody rsp)
  where
   getRequest' :: String -> Request ByteString
   getRequest' urlString =
    case parseURI urlString of
      Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
      Just u  -> mkRequest GET u

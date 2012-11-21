{--
    cabal update
    cabal install utf8-string
    cabal install http-enumerator
--}

module Main where

import Network.HTTP.Enumerator
import Network.Socket
import Data.ByteString.Lazy.UTF8


downloadString :: String -> IO String
downloadString url = fmap toString (simpleHttp url) 

main :: IO ()
main = do
    withSocketsDo (return ())
    str <- downloadString "http://example.com/" 
    putStrLn str
    
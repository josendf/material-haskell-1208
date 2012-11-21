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
downloadString url = do
    resp <- simpleHttp url
    return (toString resp) 

main :: IO ()
main = do
    withSocketsDo (return ())
    str <- downloadString "http://example.com/"
    putStrLn str
    
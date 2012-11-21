{--
    cabal update
    cabal install utf8-string
    cabal install http-enumerator
    cabal install tagsoup
--}

module Main where

import Control.Applicative
import Data.ByteString.Lazy.UTF8
import Data.Maybe
import Network.Socket
import Network.HTTP.Enumerator
import Network.URI
import Text.HTML.TagSoup

downloadString :: String -> IO String
downloadString url = pure toString <*> (simpleHttp url) 

parseHtml :: String -> [Tag String]
parseHtml = canonicalizeTags . parseTags 

canFollow :: Tag String -> Bool
canFollow tag = fromAttrib "rel" tag /= "nofollow"

isLink :: Tag String -> Bool
isLink = isTagOpenName "a"

filterLinks :: [Tag String] -> [Tag String]
filterLinks = filter canFollow . 
              filter isLink

selectHrefs :: [Tag String] -> [String]
selectHrefs = map (fromAttrib "href")

canonicalize :: String -> String -> Maybe String
canonicalize referer path = do
  r <- parseURI referer
  p <- parseURIReference path
  u <- nonStrictRelativeTo p r
  return (uriToString id u "")

cleanUrls :: String -> [String] -> [String]
cleanUrls url = map fromJust .
                filter isJust .
                map (canonicalize url)

main :: IO ()
main = do
    withSocketsDo (return ())

    let url = "http://example.com/"

    putStrLn ""
    putStrLn "Downloading ..."
    putStrLn ""
    str <- downloadString url

    putStrLn ""
    putStrLn "Parsing html ..."
    putStrLn ""
    let tags = parseHtml str 
    putStrLn $ show tags

    putStrLn ""
    putStrLn "Filtering links ..."
    putStrLn ""
    let links = filterLinks tags
    putStrLn $ show links

    putStrLn ""
    putStrLn "Selecting hrefs ..."
    putStrLn ""
    let hrefs = selectHrefs links
    putStrLn $ show hrefs

    putStrLn ""
    putStrLn "Cleaning urls ..."
    putStrLn ""
    let urls = cleanUrls url hrefs
    putStrLn $ show urls

    putStrLn ""
    putStrLn "Done ..."
    putStrLn ""

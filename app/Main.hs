{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Arch

import Text.XML (Document)
import Text.XML.Cursor
import Data.Either (rights)
import Data.List.Split (chunksOf)
import Data.Aeson
import Data.String.Conv (toS)


type Bob = Document -> Either ([Cursor], String) [Cursor]

parseDoc :: Bob
parseDoc doc = Right [fromDocument doc]
    >>= extract "1" (($/ element "body"))
    >>= extract "2" (($/ element "div"))
    >>= extract "3" (attributeIs "id" "content")
    >>= extract "5" (($/ element "div"))
    >>= extract "6" (($/ element "table"))
    >>= extract "9" (($/ element "tbody"))
    >>= extract "10" (($/ element "tr"))
    >>= extract "11b" ($/ element "th")
    >>= extract "11" (contentIs "core")
    >>= extract "12" (parent)
    >>= extract "13" ($/ element "td")
    >>= extract "14" ($/ element "div")
    >>= extract "15" ($/ element "table")
    >>= extract "16" ($/ element "tbody")
    >>= extract "16" ($/ element "tr")
    >>= extract "16" ($/ element "td")

parseDoc2 :: Bob
parseDoc2 doc = failIfEmpty ([], "No root") [fromDocument doc]
    >>= extract "1" ($/ element "body")
    >>= extract "2" ($/ element "div")
    >>= extract "3" (attributeIs "class" "content")
    >>= extract "4" ($/ element "h1")
    >>= extract "5" (contentIs "bob")
    >>= extract "6" (parent)
    >>= extract "7" ($/ element "p")


main :: IO ()
main = do
    doc <- getDocumentFile "tmp/archlinux.html"
    case parseDoc doc of
      Left (x, errorString) -> do
        _ <- printCursor x
        -- mapM print x
        print  $ "failure:" ++ errorString
      Right x -> do
        -- print $ map getListOfPackages $ take 1 $ chunksOf 2 x
        let packages = rights $ map (getListOfPackages . listToTuple) $ chunksOf 2 x
        --print $ packages
        writeFile "abc.json" (toS $ encode packages)
        print ("Success" :: String)

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

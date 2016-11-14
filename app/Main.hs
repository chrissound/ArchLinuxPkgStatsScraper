{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Arch

import Text.XML (Document)
import Text.XML.Cursor
import Data.Either (rights)
import Data.List.Split (chunksOf)
import Data.Aeson
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics (Generic)

parseArchDoc :: String -> (Document -> Either ([Cursor], String) [Cursor])
parseArchDoc alias = (\doc-> Right [fromDocument doc]
    >>= extract "1" (($/ element "body"))
    >>= extract "2" (($/ element "div"))
    >>= extract "3" (attributeIs "id" "content")
    >>= extract "5" (($/ element "div"))
    >>= extract "6" (($/ element "table"))
    >>= extract "9" (($/ element "tbody"))
    >>= extract "10" (($/ element "tr"))
    >>= extract "11b" ($/ element "th")
    >>= extract "11" (contentIs alias)
    >>= extract "12" (parent)
    >>= extract "13" ($/ element "td")
    >>= extract "14" ($/ element "div")
    >>= extract "15" ($/ element "table")
    >>= extract "16" ($/ element "tbody")
    >>= extract "16" ($/ element "tr")
    >>= extract "16" ($/ element "td"))

data PackagesStats = PackagesStat
  { core :: [[Text]]
  , extra :: [[Text]]
  } deriving (Generic)

instance ToJSON PackagesStats

main :: IO ()
main = do
    doc <- getDocumentFile "tmp/archlinux.html"
    let coreParse = parseArchDoc "extra" $ doc
    case coreParse of
      Left (x, errorString) -> do
        putStr $ printCursor x
        print  $ "failure:" ++ errorString
      Right x -> do
        -- print $ map getListOfPackages $ take 1 $ chunksOf 2 x
        let packages = rights $ map (getListOfPackages . listToTuple) $ chunksOf 2 x
        let packageStats = PackagesStat packages [["abc"]]
        writeFile "abc.json" (toS $ encode packageStats)
        print ("Success" :: String)

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

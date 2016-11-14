{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Arch

import Text.XML (Document)
import Text.XML.Cursor
import Data.Either (rights, isRight)
import Data.List.Split (chunksOf)
import Data.Aeson
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics (Generic)

type ParsedDocument = Either ([Cursor], String) [Cursor]
type DocumentParse = Document -> ParsedDocument

parseArchDoc :: String -> (DocumentParse)
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

extractRights :: [Cursor] -> [[Text]]
extractRights x = rights $ map (getListOfPackages . listToTuple) $ chunksOf 2 x

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Not a Right value"


errorIfLeft :: ParsedDocument -> IO ()
errorIfLeft (Left (x, errorString)) = do
  putStr $ printCursor x
  print  $ "failure:" ++ errorString
errorIfLeft _ = return ()

main :: IO ()
main = do
    doc <- getDocumentFile "tmp/archlinux.html"
    let coreParse = parseArchDoc "core" $ doc
    let extraParse = parseArchDoc "extra" $ doc
    errorIfLeft coreParse
    errorIfLeft extraParse
    case and $ map isRight [coreParse, extraParse] of
      True -> do
        let corePackages = extractRights $ fromRight coreParse
        let extraPackages = extractRights $ fromRight extraParse
        let packageStats = PackagesStat corePackages extraPackages
        writeFile "abc.json" (toS $ encode packageStats)
        print ("Success" :: String)
      False -> print ("Errors occurred" :: String)

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

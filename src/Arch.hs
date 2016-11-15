{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Arch where

import Lib

import Text.XML (Document)
import Text.XML.Cursor
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

getListOfPackages :: (Cursor, Cursor) -> Either ([Cursor], [Maybe Text]) [Text]
getListOfPackages (cursor, cursorb) = do
  let packageName = getContent $ node $ cursor
  let percentage =
        case getPercentageFromPackageCursor cursorb of
          Right y ->
            case getContent . node $ head y of
              Just z -> Just $ filterText z
              _ -> Nothing
          _ -> Nothing
  let maybeValues = [packageName, percentage]
      values = concatMap g maybeValues
    in case length maybeValues == length values of
        True -> Right values
        False -> Left ([cursor], maybeValues)

g :: Maybe a -> [a]
g (Just x) = [x]
g _ = []

getPercentageFromPackageCursor :: Cursor -> Either ([Cursor], String) [Cursor]
getPercentageFromPackageCursor cursor = Right [cursor]
  >>= extract "1" ($/ element "table")
  >>= extract "2" ($/ element "tbody")
  >>= extract "3" ($/ element "tr")
  >>= extract "4" ($/ element "td")
  >>= extract "5" (followingSibling)
  >>= extract "6" (followingSibling)

type DocumentParse = Document -> ParsedDocument

type ParsedDocument = Either ([Cursor], String) [Cursor]

data PackagesStats = PackagesStat
  { core :: [[Text]]
  , extra :: [[Text]]
  , community :: [[Text]]
  , multilib :: [[Text]]
  , unknown :: [[Text]]
  } deriving (Generic)

instance ToJSON PackagesStats

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

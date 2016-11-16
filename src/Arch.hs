{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Arch where

import Lib

import Text.XML (Document)
import Text.XML.Cursor
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)


getListOfPackages :: (Cursor, Cursor) -> CursorParseEither ([Maybe Text]) Text
getListOfPackages (cursor, cursorb) = do
  let packageName = case value == "" of
        True -> Nothing
        False -> Just value
        where value = getContent $ node $ cursor
  let percentage = case getPercentageFromPackageCursor cursorb of
          Right (y:_) -> Just . filterText  . getContent . node $ y
          _ -> Nothing
  justsToRight packageName percentage ([cursor], [packageName, percentage])

justsToRight :: Maybe a -> Maybe a -> CursorParseLeft b -> CursorParseEither b a
justsToRight (Just x) (Just y) _ = Right [x, y]
justsToRight _ _ failure = Left failure

getPercentageFromPackageCursor :: Cursor -> CursorParseEither String Cursor
getPercentageFromPackageCursor cursor = Right [cursor]
  >>= extract "1" ($/ element "table")
  >>= extract "2" ($/ element "tbody")
  >>= extract "3" ($/ element "tr")
  >>= extract "4" ($/ element "td")
  >>= extract "5" (followingSibling)
  >>= extract "6" (followingSibling)

type CursorParseLeft a = ([Cursor], a)
type CursorParseEither failType succListType = Either (CursorParseLeft failType) [succListType]

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

parseArchDoc :: Text-> (DocumentParse)
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

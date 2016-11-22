{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Arch where

import Lib
import Text.XML.Cursor
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data PackagesStats = PackagesStats
  { core :: [[Text]]
  , extra :: [[Text]]
  , community :: [[Text]]
  , multilib :: [[Text]]
  , unknown :: [[Text]]
  } deriving (Generic, Show)

instance ToJSON PackagesStats

getListOfPackages :: (Cursor, Cursor) -> CursorParseEither ([Maybe Text]) Text
getListOfPackages (cursor, cursorb) = do
  let packageName = case value == "" of
        True -> Nothing
        False -> Just value
        where value = getContent $ node $ cursor
  let percentage = case getPercentageFromPackageCursor cursorb of
          Right (y:_) -> Just . filterText  . getContent . node $ y
          _ -> Nothing
  case justsToRight <$> packageName <*> percentage of
    Just x -> x
    Nothing -> Left ([cursor], [packageName, percentage])

justsToRight :: a -> a -> CursorParseEither b a
justsToRight x y = Right [x, y]

getPercentageFromPackageCursor :: Cursor -> CursorParseEither String Cursor
getPercentageFromPackageCursor cursor = Right [cursor]
  >>= extract "1" ($/ element "table")
  >>= extract "2" ($/ element "tbody")
  >>= extract "3" ($/ element "tr")
  >>= extract "4" ($/ element "td")
  >>= extract "5" (followingSibling)
  >>= extract "6" (followingSibling)


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

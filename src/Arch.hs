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
  let required = [packageName, percentage] in
    case sequence required of
      Just x -> Right x
      Nothing -> Left ([cursor], required)

getPercentageFromPackageCursor :: Cursor -> CursorParseEither String Cursor
getPercentageFromPackageCursor cursor = Right [cursor]
  >>= extract "1" ($/ element "table")
  -- >>= extract "2" ($/ element "tbody")
  >>= extract "3" ($/ element "tr")
  >>= extract "4" ($/ element "td")
  >>= extract "5" (followingSibling)
  >>= extract "6" (followingSibling)


parseArchDoc :: Text-> (DocumentParse)
parseArchDoc alias = (\doc-> Right [fromDocument doc]
    >>= extract "ParseArchDoc E1" ($/ element "body")
    >>= extract "ParseArchDoc E2" ($/ element "div")
    >>= extract "ParseArchDoc E3" (attributeIs "id" "content")
    >>= extract "ParseArchDoc E5" ($/ element "div")
    >>= extract "ParseArchDoc E6" ($/ element "table")
    -- >>= extract "ParseArchDoc E9" ($/ element "tbody")
    >>= extract "ParseArchDoc E10" ($/ element "tr")
    >>= extract "ParseArchDoc E11b" ($/ element "th")
    >>= extract "ParseArchDoc E11" (contentIs alias)
    >>= extract "ParseArchDoc E12" (parent)
    >>= extract "ParseArchDoc E13" ($/ element "td")
    >>= extract "ParseArchDoc E14" ($/ element "div")
    >>= extract "ParseArchDoc E15" ($/ element "table")
    -- >>= extract "ParseArchDoc E16" ($/ element "tbody")
    >>= extract "ParseArchDoc E16" ($/ element "tr")
    >>= extract "ParseArchDoc E16" ($/ element "td"))

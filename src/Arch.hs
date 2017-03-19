{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Arch
  (PackagesStats (..)
  ,PackageStat
  ,ParsedDocument
  ,getListOfPackages
  ,parseArchDoc
  ,getPackagesStats
  ,getPackages
  ,searchPackageStats)
where

import XMLPrint
import Lib
import Text.XML.Cursor
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, decode)
import qualified Data.ByteString.Lazy as Lazy
import Text.XML (Document)
import Data.List (find)

type PackageStat = (Text, Float)

type DocumentParse = Document -> ParsedDocument
type ParsedDocument = CursorParseEither String [Cursor]

data PackagesStats = PackagesStats
  { core :: [PackageStat]
  , extra :: [PackageStat]
  , community :: [PackageStat]
  , multilib :: [PackageStat]
  , unknown :: [PackageStat]
  } deriving (Generic, Show)

instance ToJSON PackagesStats
instance FromJSON PackagesStats

getPackages :: PackagesStats -> [PackageStat]
getPackages s = concat [
  core s,
  extra s,
  community s,
  multilib s,
  unknown s
  ]

getPackagesStats :: String -> IO (Maybe PackagesStats)
getPackagesStats x = decode <$> Lazy.readFile x

searchPackageStats :: PackagesStats -> Text -> Maybe PackageStat
searchPackageStats packageStats package = find ((== package) . fst) $ getPackages packageStats

getListOfPackages :: (Cursor, Cursor) -> CursorParseEither ([Maybe Text]) PackageStat
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
      Just (pname:pperc:[]) -> Right (pname, 0.01 * (read $ filterFloatString . unpack $ pperc))
      _ -> Left ([cursor], required)

getPercentageFromPackageCursor :: Cursor -> CursorParseEither String [Cursor]
getPercentageFromPackageCursor cursor = Right [cursor]
  >>= extract "1" ($/ element "table")
  -- >>= extract "2" ($/ element "tbody")
  >>= extract "3" ($/ element "tr")
  >>= extract "4" ($/ element "td")
  >>= extract "5" (followingSibling)
  >>= extract "6" (followingSibling)


parseArchDoc :: Text-> DocumentParse
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

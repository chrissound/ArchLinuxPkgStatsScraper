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
import Data.Aeson (ToJSON, FromJSON, decode, eitherDecode)
import qualified Data.ByteString.Lazy as Lazy
import Text.XML (Document)
import Data.List (find)
import Data.Char (isDigit)

type PackageStat = (Text, Int)

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

getPackagesStats :: String -> IO (Either String PackagesStats)
getPackagesStats x = do
  fc <- Lazy.readFile x
  return $ eitherDecode fc

searchPackageStats :: PackagesStats -> Text -> Maybe PackageStat
searchPackageStats packageStats package = find ((== package) . fst) $ getPackages packageStats


getListOfPackages :: (Cursor, Cursor) -> CursorParseEither ([Maybe Text]) PackageStat
getListOfPackages (cursor, cPkgValue) = do
  let packageName = case (getContent . node $ cursor) of
        ("") -> Nothing
        v -> Just v
  let percentage = case (getPercentageFromPackageCursor cPkgValue) of
          Right (c:_) -> Just . filterText . head $ attribute "title" c
          _ -> Nothing
  let required = [packageName, percentage] in
    case sequence required of
      Just (pname:pperc:[]) -> Right (pname, parsePkgValue pperc)
      _ -> Left ([cursor], required)
  where
    -- "35,351 of 35,769" -> 35351
    parsePkgValue :: Text -> Int
    parsePkgValue = read . filter isDigit . head . words . unpack

getPercentageFromPackageCursor :: Cursor -> CursorParseEither String [Cursor]
getPercentageFromPackageCursor cursor = Right [cursor]
  >>= extract "ParseArchPercentage E1" ($/ element "table")
  -- >>= extract "2" ($/ element "tbody")
  >>= extract "ParseArchPercentage E3" ($/ element "tr")
  >>= extract "ParseArchPercentage E4" ($/ element "td")
  >>= extract "ParseArchPercentage E5" ($/ element "div")
  >>= extract "ParseArchPercentage E6" (hasAttribute "title")

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

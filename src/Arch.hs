{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Arch where

import Lib

import Text.XML.Cursor
import Data.Text (Text)

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

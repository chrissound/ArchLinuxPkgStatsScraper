{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Text.XML
import Text.XML.Cursor
import Data.Maybe (mapMaybe, fromJust, isJust)
import Data.Either (rights)
import Data.Text (unpack, Text)
import Safe (headMay)
import Data.List.Split (chunksOf)

failIfEmpty :: error -> [b] -> Either error [b]
failIfEmpty reason [] = Left reason
failIfEmpty _      xs = Right xs

--extract :: errorString -> ([a] -> [b]) -> Either ([b], ) [b]
--tract es f x = failIfEmpty (x, es) $ x >>= f
extract :: t -> (a -> [b]) -> [a] -> Either ([a], t) [b]
extract es f x = failIfEmpty (x, es) $ x >>= f

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

contentIs :: String -> Cursor -> [Cursor]
contentIs text cursor = case cursorContentIs text cursor of
  False -> []
  True -> [cursor]

cursorContentIs :: String -> Cursor -> Bool
cursorContentIs text cursor = case getContent $ node cursor of
  Just x -> unpack x == text
  Nothing -> False

getContent :: Node -> (Maybe Text)
getContent (NodeElement e) = headMay $ mapMaybe getContent $ elementNodes e
getContent (NodeContent c) = Just c
getContent _ = Just "???"

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
    --doc2 <- getDocumentFile "bob.html"
    --let contents = do
    -- let test2 = parseDoc2 doc2
    -- case test2 of
    --   Left (x, errorString) -> do
    --     printCursor x
    --     mapM print x
    --     print  $ "failure:" ++ errorString
    --   Right x -> do
    --     printCursor x
    --     print "success"
    let test2 = parseDoc doc
    case test2 of
      Left (x, errorString) -> do
        _ <- printCursor x
        -- mapM print x
        print  $ "failure:" ++ errorString
      Right x -> do
        -- print $ map getListOfPackages $ take 1 $ chunksOf 2 x
        print $ rights $ map (getListOfPackages . listToTuple) $ chunksOf 2 x
        print ("success" :: [Char])

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

getPercentageFromPackageCursor :: Cursor -> Either ([Cursor], String) [Cursor]
getPercentageFromPackageCursor cursor = Right [cursor]
  >>= extract "1" ($/ element "table")
  >>= extract "2" ($/ element "tbody")
  >>= extract "3" ($/ element "tr")
  >>= extract "4" ($/ element "td")
  >>= extract "5" (followingSibling)
  >>= extract "6" (followingSibling)

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
  let justValues = [packageName, percentage]
      left = Left ([cursor, cursorb], justValues)
    in case (length $ filter isJust justValues) == length justValues of
        True -> Right $ map fromJust justValues
        False -> left

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Text.XML
import Text.XML.Cursor
import Data.Char (isPrint, isSpace)
import qualified Text.XML.Cursor.Generic
import Data.Text (unpack, pack, Text)
import Safe (headMay)

failIfEmpty :: error -> [b] -> Either error [b]
failIfEmpty reason [] = Left reason
failIfEmpty _      xs = Right xs

--extract :: t -> (a -> [b]) -> [a] -> Either ([a], t) [b]
--extract :: errorString -> ([a] -> [b]) -> Either ([b], ) [b]
--tract es f x = failIfEmpty (x, es) $ x >>= f
extract es f x = failIfEmpty (x, es) $ x >>= f

type Bob = Document -> Either ([Cursor], String) [Cursor]

parseDoc :: Bob
parseDoc doc = failIfEmpty ([], "No root") [fromDocument doc]
    >>= extract "1" (($/ element "body"))
    >>= extract "2" (($/ element "div"))
    >>= extract "3" (attributeIs "id" "content")
    >>= extract "5" (($/ element "div"))
    >>= extract "6" (($/ element "table"))
    >>= extract "9" (($/ element "tbody"))
    >>= extract "10" (($/ element "tr"))
    >>= extract "10" (($/ element "th"))

contentIs :: String -> Cursor -> [Cursor]
contentIs text cursor = case cursorContentIs text cursor of
  False -> []
  True -> [cursor]

cursorContentIs :: String -> Cursor -> Bool
cursorContentIs text cursor = case getContent $ node cursor of
  Just x -> unpack x == text
  Nothing -> False

getContent :: Node -> (Maybe Text)
getContent (NodeElement e) = headMay ( map (\(NodeContent t) -> t) $ elementNodes e)


parseDoc2 :: Bob
parseDoc2 doc = failIfEmpty ([], "No root") [fromDocument doc]
    >>= extract "1" ($/ element "body")
    >>= extract "2" ($/ element "div")
    >>= extract "3" (attributeIs "class" "content")
    >>= extract "4" ($/ element "h1")
    >>= extract "5" (contentIs "bob")


main :: IO ()
main = do
    doc <- getDocumentFile "archlinux.html"
    doc2 <- getDocumentFile "bob.html"
    --let contents = do
    let test2 = parseDoc2 doc2
    case test2 of
      Left (x, errorString) -> do
        printCursor x
        mapM print x
        print  $ "failure:" ++ errorString
      Right x -> do
        printCursor x
        print "success"

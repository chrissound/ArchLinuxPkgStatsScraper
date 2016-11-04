{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack, pack)
import Data.String.Utils (rstrip)
import Text.XML
import Text.XML.Cursor
import Data.Char (isPrint, isSpace)
import qualified Text.XML.Cursor.Generic

import Control.Monad.Trans.List
import Control.Monad
import Control.Monad.Trans.Class

applyLeftRight :: String -> [a] -> Either String [a]
applyLeftRight errorString x = if (length x) == 0 then Left errorString else Right x

failIfEmpty :: a -> [b] -> Either a [b]
failIfEmpty reason [] = Left reason
failIfEmpty _      xs = Right xs

parseDoc :: Document -> Either ([Cursor], String) [Cursor]
parseDoc doc = do
    root <- failIfEmpty ([], "no document") $ [fromDocument doc]
    body <- failIfEmpty (root, "no body!") $ root >>= ($/ element "body")
    divs <- failIfEmpty (body, "no divs!") $ body >>= ($/ element "div")
    contents <- failIfEmpty (divs, "no contents!") $ divs >>= attributeIs "id" "content"
    pure contents


main :: IO ()
main = do
    --doc <- makeRequest "https://www.archlinux.de/?page=PackageStatistics"
    doc <- getDocumentFile "archlinux.html"
    --let contents = do
    let test2 = parseDoc doc
    case test2 of
      Left (x, errorString) -> do
        printCursor x
        print  $ "failure:" ++ errorString
      Right x -> do
        printCursor x
        print "success"

    -- runListT $ (ListT (Just [1,2,3]) >>=(\x -> ListT $ return [x+1]))
    -- Just [2,3,4]
--     let test2 = runListT $
--                 (   ListT ((applyLeftRight "First " [1,2,3]))
--                  >>= (\_ -> ListT $ Right [])
--                  ) :: Either String [Int]
--     let test2 = runListT $
--                 (
--                     (ListT (
--                         applyLeftRight "First" (fromDocument (doc :: Document) $/ element "body")
--                     ))
--                     >>= (\_ -> (ListT (Left "abcxyz")))
--                 ) :: Either String [Cursor]
--    let test2 = runListT $
--                (
--                    (ListT $ applyLeftRight "First" (fromDocument (doc :: Document) $/ element "body"))
--                    >>= (\x -> (ListT $ applyLeftRight "Second" (x $/ element "div")))
--                    >>= (\x -> (ListT $ applyLeftRight "Third" (attributeIs "id" "content")))
--                ) :: Either String [Cursor]

--             >>= applyLeftRight "Third" (attributeIs "id" "content")
--             >>= applyLeftRight "Fourth" ($/ element "div")
--             >>= applyLeftRight "Fifth" (attributeIs "class" "box")

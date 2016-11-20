{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-))
import Network.HTTP.Conduit (tlsManagerSettings, http, newManager, parseUrlThrow, responseBody)
import qualified Text.HTML.DOM as THX (readFile, sinkDoc)
import Text.XML as TX (Document)
import Data.Text as DT (Text, unpack)
import qualified Data.Text as DT (strip, words, unwords)
import Data.String.Utils (strip, rstrip)
import Text.XML
import Text.XML.Cursor (node, Cursor)
import qualified Text.XML.Cursor.Generic as XMLG

type CursorParseLeft a = ([Cursor], a)
type CursorParseEither failType succListType = Either (CursorParseLeft failType) [succListType]
type DocumentParse = Document -> ParsedDocument
type ParsedDocument = CursorParseEither String Cursor

getDocumentFile :: FilePath -> IO Document
getDocumentFile path = do
  THX.readFile path

makeRequest :: String -> IO Document
makeRequest url = do
  request <- parseUrlThrow url

  manager <- newManager tlsManagerSettings

  runResourceT $ do
    -- Actually make the request
    response <- http request manager
    -- Extract the response body.
    let body = responseBody response
    -- Parse the body as HTML.
    body $$+- THX.sinkDoc

formatChildString :: String -> String
formatChildString string = rstrip . unlines . map prefixStringIndent $ lines string

prefixString :: String -> String -> String
prefixString prefix string = prefix ++ string

prefixStringIndent :: String -> String
prefixStringIndent = prefixString "    "

filterString :: String -> String
filterString = strip . unwords . words

filterText :: Text -> Text
filterText = DT.strip . DT.unwords . DT.words

cursorContentIs :: Text -> Cursor -> Bool
cursorContentIs text cursor = (getContent . node $ cursor) == text

contentIs :: Text -> Cursor -> [Cursor]
contentIs text cursor = case cursorContentIs text cursor of
  False -> []
  True -> [cursor]

getContent :: Node -> Text
getContent (NodeElement e) = mconcat . map getContent $ elementNodes e
getContent (NodeContent c) = c
getContent _ = "Ignored: Node Instruction / Node Comment"

failIfEmpty :: error -> [b] -> Either error [b]
failIfEmpty reason [] = Left reason
failIfEmpty _      xs = Right xs

extract :: t -> (a -> [b]) -> [a] -> Either ([a], t) [b]
extract es f x = failIfEmpty (x, es) $ x >>= f

printCursor :: [XMLG.Cursor Node] -> String
printCursor cursor = concat ( map (printBasicNode . node) cursor)

printBasicNode :: Node -> String
printBasicNode nodePrint@(NodeElement element) = mainNode ++ "Chlidren: \n" ++ children where
  mainNode = (unlines [
      "Main node: "
      , printBasicNodeElements nodePrint
    ])
  children = concatMap unlines [
      map (formatChildString . printBasicNodeElements) (elementNodes element)
    ]
printBasicNode (NodeContent content) = prefixString "Content:" . filterString $ unpack content
printBasicNode _ = "Ignored: Node Instruction / Node Comment"

printBasicNodeElements :: Node -> String
printBasicNodeElements (NodeElement element) = unlines
  [
    "element: " ++ (unpack . nameLocalName $ elementName element)
    , prefixStringIndent "attributes: " ++ (show $ elementAttributes element)
 ]

printBasicNodeElements (NodeContent content) = "element content: " ++ (filterString . unpack $ content)
printBasicNodeElements _ = "Ignored: Node Instruction / Node Comment"

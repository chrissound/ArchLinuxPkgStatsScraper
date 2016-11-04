module Lib where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-))
import Network.HTTP.Conduit (tlsManagerSettings, http, newManager, parseUrlThrow, responseBody)
import qualified Text.HTML.DOM as THX (readFile, sinkDoc)
import Text.XML as TX (Document)
import Data.Default

import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack, pack)
import Data.String.Utils (rstrip)
import Text.XML
import Text.XML.Cursor
import Data.Char (isPrint, isSpace)
import qualified Text.XML.Cursor.Generic

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

printCursor cursor = mapM putStrLn ( map (printBasicNode . node) cursor)

printBasicNode :: Node -> String
printBasicNode nodePrint@(NodeElement element) = mainNode ++ "Chlidren: \n" ++ children where
  mainNode = (unlines [
      "Main node: "
      , printBasicNodeElements nodePrint
    ])
  children = concatMap unlines [
      map (formatChildString . printBasicNodeElements) (elementNodes element)
    ]

formatChildString :: String -> String
formatChildString string = rstrip $ unlines $ map (prefixString "    ")  $ lines string

prefixString :: String -> String -> String
prefixString prefix string = prefix ++ string

printBasicNodeElements :: Node -> String
printBasicNodeElements (NodeElement element) = unlines
  [
    "element: " ++ (unpack $ nameLocalName $ elementName element)
    , "    attributes: " ++ (show $ elementAttributes element)
  ]

printBasicNodeElements (NodeContent content) = "element content: " ++ contentString where
  contentString = show contentStripped
  contentStripped = filter (not . isSpace) $ unpack content
printBasicNodeElements (NodeComment comment) = "element comment: " ++ commentString where
  commentString = show comment
printBasicNodeElements  a = "element ???:" ++ show a

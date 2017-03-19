{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-))
import Network.HTTP.Conduit (tlsManagerSettings, http, newManager, parseUrlThrow, responseBody)
import qualified Text.HTML.DOM as THX (readFile, sinkDoc)
import Data.Text as DT (Text)
import Text.XML
import Text.XML.Cursor (node, Cursor)

type CursorParseLeft a = ([Cursor], a)
type CursorParseEither e a = Either (CursorParseLeft e) a

getDocumentFile :: FilePath -> IO Document
getDocumentFile path = THX.readFile path

makeRequest :: String -> IO Document
makeRequest url = do
  request <- parseUrlThrow url
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager
    let body = responseBody response
    body $$+- THX.sinkDoc

contentIs :: Text -> Cursor -> [Cursor]
contentIs text cursor = if (getContent . node $ cursor) == text
  then [cursor]
  else []

getContent :: Node -> Text
getContent (NodeElement e) = mconcat . map getContent $ elementNodes e
getContent (NodeContent c) = c
getContent _ = "Ignored: Node Instruction / Node Comment"

extract :: e -> (a -> [b]) -> [a] -> Either ([a], e) [b]
extract es f x = case (x >>= f) of
  [] -> Left (x, es)
  y -> Right y

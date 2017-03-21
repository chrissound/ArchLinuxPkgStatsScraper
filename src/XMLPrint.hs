module XMLPrint where

import Text.XML
import Data.Text as DT (Text)
import Data.String.Utils (strip)
import qualified Data.Text as DT (strip, words, unwords)
import Text.XML.Cursor (node)
import Data.Text as DT (unpack)
import qualified Text.XML.Cursor.Generic as XMLG
import Data.String.Utils (rstrip)

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
printBasicNode (NodeContent content) = (++ "Content:") . filterString $ unpack content
printBasicNode _ = "Ignored: Node Instruction / Node Comment"

printBasicNodeElements :: Node -> String
printBasicNodeElements (NodeElement element) = unlines
  [
    "element: " ++ (unpack . nameLocalName $ elementName element)
    , prefixStringIndent "attributes: " ++ (show $ elementAttributes element)
 ]

printBasicNodeElements (NodeContent content) = "element content: " ++ (filterString . unpack $ content)
printBasicNodeElements _ = "Ignored: Node Instruction / Node Comment"

formatChildString :: String -> String
formatChildString string = rstrip . unlines . map prefixStringIndent $ lines string

prefixStringIndent :: String -> String
prefixStringIndent = (++ "    ")

filterString :: String -> String
filterString = strip . unwords . words

filterFloatString :: String -> String
filterFloatString = filterString . filter (/= '%')

filterText :: Text -> Text
filterText = DT.strip . DT.unwords . DT.words

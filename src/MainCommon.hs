module MainCommon where

import Lib
import Arch
import XMLPrint

import System.Environment (getArgs)
import Data.List.Split (chunksOf)
import Text.XML (Document)

getDocumentByArgs :: IO Text.XML.Document
getDocumentByArgs = do
    args <- getArgs
    case args of
      (sourceType:path:[]) ->
        if sourceType == "--file"
        then getDocumentFile path
        else if sourceType == "--url"
        then makeRequest "https://www.archlinux.de/?page=PackageStatistics"
        else error "First paramater must be --file or --url"
      _ -> error "Invalid parameters passed"

extractRights :: ParsedDocument -> Either String [PackageStat]
extractRights (Right x) = case sequence . map (getListOfPackages . listToTuple) $ chunksOf 2 x of
   Right r -> return r
   Left (_, required) -> Left $ "Package not parsed correctly: " ++ show required
extractRights (Left (x, errorString)) = Left
  $ "Unable to parse package type, error occurred:"
  ++ errorString
  ++ "\n\n"
  ++ printCursor x

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Arch

import Data.List.Split (chunksOf)
import Data.String.Conv (toS)
import Data.Aeson (encode)
import System.Environment (getArgs)
import Text.XML (Document)

extractRights :: ParsedDocument -> Either String [PackageStat]
extractRights (Right x) = case sequence values of
   Right r -> Right r
   Left (_, required) -> Left $ "Package not parsed correctly: " ++ show required
  where
    values = map (getListOfPackages . listToTuple) $ chunksOf 2 x

extractRights (Left (x, errorString)) = Left $ "Unable to parse package type, error occurred:" ++ errorString ++ "\n\n" ++ printCursor x

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

main :: IO ()
main = do
    doc <- getDocumentByArgs
    let packageStats = PackagesStats
          <$> extractRights ( parseArchDoc "core" $ doc )
          <*> extractRights ( parseArchDoc "extra" $ doc )
          <*> extractRights ( parseArchDoc "community" $ doc )
          <*> extractRights ( parseArchDoc "multilib" $ doc )
          <*> extractRights ( parseArchDoc "unknown" $ doc )

    case packageStats of
      Right pkgs -> do
        writeFile "packageStatistics.json" . toS $ encode pkgs
        print ("Success" :: String)
      Left l -> putStrLn l

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

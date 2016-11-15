{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Arch

import Text.XML.Cursor
import Data.Either (rights, isRight)
import Data.List.Split (chunksOf)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Aeson (encode)

extractRights :: [Cursor] -> [[Text]]
extractRights x = rights $ map (getListOfPackages . listToTuple) $ chunksOf 2 x

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Not a Right value"


errorIfLeft :: ParsedDocument -> IO ()
errorIfLeft (Left (x, errorString)) = do
  putStr $ printCursor x
  print  $ "failure:" ++ errorString
errorIfLeft _ = return ()

main :: IO ()
main = do
    doc <- getDocumentFile "tmp/archlinux.html"
    let coreParse = parseArchDoc "core" $ doc
    let extraParse = parseArchDoc "extra" $ doc
    let communityParse = parseArchDoc "community" $ doc
    let multilibParse = parseArchDoc "multilib" $ doc
    let unknownParse = parseArchDoc "unknown" $ doc
    let packagesList = [coreParse, extraParse, communityParse, multilibParse, unknownParse]
    mapM_ errorIfLeft packagesList
    case and $ map isRight packagesList of
      True -> do
        let corePackages = extractRights $ fromRight coreParse
        let extraPackages = extractRights $ fromRight extraParse
        let communityPackages = extractRights $ fromRight communityParse
        let multilibPackages = extractRights $ fromRight multilibParse
        let unknownPackages = extractRights $ fromRight unknownParse
        let packageStats = PackagesStat corePackages extraPackages communityPackages multilibPackages unknownPackages
        writeFile "abc.json" (toS $ encode packageStats)
        print ("Success" :: String)
      False -> print ("Errors occurred" :: String)

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

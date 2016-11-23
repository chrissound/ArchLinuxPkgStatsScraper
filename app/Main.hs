{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Arch

import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.String.Conv (toS)
import Data.Aeson (encode)
--import Data.Either (rights, isRight)


hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a

extractRights :: ParsedDocument -> Either String [[Text]]
extractRights (Right x) = case sequence values of
   Right r -> Right r
   Left (_, required) -> Left $ "Package not parsed correctly: " ++ show required
  where
    values = (map (getListOfPackages . listToTuple) $ chunksOf 2 x) :: [CursorParseEither ([Maybe Text]) Text]

extractRights (Left (x, errorString)) = Left $ "Unable to parse package type, error occurred:" ++ errorString ++ "\n\n" ++ printCursor x

main :: IO ()
main = do
    doc <- makeRequest "https://www.archlinux.de/?page=PackageStatistics"
    --doc <- getDocumentFile "tmp/archlinux.html"
    let packageStats = PackagesStats
          <$> extractRights ( parseArchDoc "core" $ doc )
          <*> extractRights ( parseArchDoc "extra" $ doc )
          <*> extractRights ( parseArchDoc "community" $ doc )
          <*> extractRights ( parseArchDoc "multilib" $ doc )
          <*> extractRights ( parseArchDoc "unknown" $ doc )

    case packageStats of
      Right pkgs -> do
        writeFile "packageStatistics.json" (toS $ encode pkgs)
        print ("Success" :: String)
      --Left l -> putStrLn . printpkgs $ l
      Left l -> putStrLn l

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

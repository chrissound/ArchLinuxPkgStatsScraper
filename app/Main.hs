{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Arch

import Text.XML.Cursor
import Data.Either (rights)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.String.Conv (toS)
import Data.Aeson (encode)

extractRights :: Either e [Cursor] -> Either e [[Text]]
extractRights (Right x) = Right $ rights $ map (getListOfPackages . listToTuple) $ chunksOf 2 x
extractRights (Left e) = Left e

printpkgs :: CursorParseLeft String -> String
printpkgs (x, errorString) =
  printCursor x ++ "failure:" ++ errorString

main :: IO ()
main = do
    doc <- getDocumentFile "tmp/archlinux.html"
    let packageStats = PackagesStats
          <$> extractRights ( parseArchDoc "core" $ doc )
          <*> extractRights ( parseArchDoc "extra" $ doc )
          <*> extractRights ( parseArchDoc "community" $ doc )
          <*> extractRights ( parseArchDoc "multilib" $ doc )
          <*> extractRights ( parseArchDoc "unknown" $ doc )

    case packageStats of
      Right pkgs -> do
        writeFile "abc.json" (toS $ encode pkgs)
        print ("Success" :: String)
      Left l -> putStrLn . printpkgs $ l

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

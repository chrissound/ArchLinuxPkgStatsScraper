{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Arch

import Text.XML.Cursor
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.String.Conv (toS)
import Data.Aeson (encode)
--import Data.Either (rights, isRight)


hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a

extractRights :: Either e [Cursor] -> Either String [[Text]]
extractRights (Right x) = case sequence . map hush $ values of
   Just  r -> Right r
   Nothing -> Left "Not all packages correct"
  where
    values = (map (getListOfPackages . listToTuple) $ chunksOf 2 x)

extractRights (Left _) = Left "test2"

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
      --Left l -> putStrLn . printpkgs $ l
      Left l -> print l

listToTuple :: [a] -> (a, a)
listToTuple (a:a':[]) = (a, a')
listToTuple _ = error ""

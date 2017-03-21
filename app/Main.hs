{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Arch
import MainCommon

import Data.String.Conv (toS)
import Data.Aeson (encode)

main :: IO ()
main = do
    doc <- getDocumentByArgs
    let packageStats = PackagesStats
          <$> f "core"
          <*> f "extra"
          <*> f "community"
          <*> f "multilib"
          <*> f "unknown"
          where f x = extractRights ( parseArchDoc x $ doc )
    case packageStats of
      Right pkgs -> do
        writeFile "packageStatistics.json" . toS $ encode pkgs
        print ("Success" :: String)
      Left l -> putStrLn l

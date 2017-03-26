{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Hspec
import MainCommon
import Data.Either (isRight)
import Data.Maybe (isJust)
import Data.Aeson (encode)
import Data.String.Conversions
import System.Environment

import Arch

parseDocRaw :: IO (Either String PackagesStats)
parseDocRaw = do
  doc <- getDocumentByArgs
  let f x = extractRights ( parseArchDoc x $ doc )
  return $ PackagesStats
        <$> f "core"
        <*> f "extra"
        <*> f "community"
        <*> f "multilib"
        <*> f "unknown"

main :: IO ()
main = do
  let pkgJsonPath = "packageStatistics.json"
  args <- getArgs
  withArgs [] . hspec $ do
    describe "parseDocRaw" $ do
      it "" $ do
        parsedDoc <- withArgs args $ parseDocRaw
        case parsedDoc of
          Right x -> writeFile pkgJsonPath . convertString $ encode x
          Left x -> print x
        shouldSatisfy parsedDoc (isRight)
    describe "getPackagesStats" $ do
      it "" $ do
        statisticsStore <- Arch.getPackagesStats pkgJsonPath
        shouldSatisfy statisticsStore (isJust)

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-unused-imports #-}
module Spec where

--import Test.QuickCheck
import Test.Hspec
import MainCommon
--import Control.Monad.Trans.Class
import Data.Either (isRight, isLeft)
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
  args <- getArgs
  withArgs [] . hspec $ do
    describe "parseDocRaw" $ do
      it "" $ do
        parsedDoc <- withArgs args $ parseDocRaw
        writeFile "packageStatistics.json" . convertString $ encode parsedDoc
        shouldSatisfy parsedDoc (isRight)
    describe "getPackagesStats" $ do
      it "" $ do
        statisticsStore <- Arch.getPackagesStats "packageStatistics.json"
        shouldSatisfy statisticsStore (isJust)

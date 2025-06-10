{-|
Module      : TestUtil
Description : Test functions for the Util module.
Copyright   : (c) IOcrafts, 2024-present
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module defines functions for testing functions within the Util module.
-}

module TestUtil
    ( utilTests
    ) where

import           Data.Time

import           System.Directory ( doesFileExist )
import           System.IO.Temp   ( withTempFile )

import           Test.Tasty       ( TestTree, testGroup )
import           Test.Tasty.HUnit ( testCase, (@?=) )

import           Util             ( createFile, toDate, toEpoch )

utilTests :: TestTree
utilTests =
    testGroup "Util tests"
      [ testCase "epoch zero" $ do
          let date = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
          -- Expected epoch time for the above date is 0
          toEpoch date @?= 0

      , testCase "toEpoch" $ do
          let date2020 = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)
          -- Expected epoch time for January 1, 2020.
          let expectedEpoch2020 = 1577836800
          toEpoch date2020 @?= expectedEpoch2020

      , testCase "createFile" $ do
          withTempFile "/tmp" "known_hosts" $ \tmpFile _ -> do
            -- Create a temporary file
            createFile tmpFile
            -- Check if the file exists
            exists <- doesFileExist tmpFile
            exists @?= True

      , testCase "toDate - correct format" $ do
          let date = "2022-01-01 00:00 UTC"
          -- Expected date for the above string is January 1, 2022, 00:00:00 UTC
          let expectedDate = UTCTime (fromGregorian 2022 1 1) (secondsToDiffTime 0)
          -- Convert the string to a UTCTime value
          toDate date @?= expectedDate

      , testCase "toDate - incorrect format" $ do
          let date = "2022-01-01 UTC"
          -- Expected date for the above string is January 1, 2022, 00:00:00 UTC
          let expectedDate = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
          -- Convert the string to a UTCTime value
          toDate date @?= expectedDate
      ]

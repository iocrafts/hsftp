{-|
Module      : TestUtil
Description : Test functions for the Util module.
Copyright   : (c) Maurizio Dusi, 2024
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

import           Util             ( createFile, toEpoch )

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
          ]

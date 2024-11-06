{-|
Module      : TestReader
Description : Test functions for the Reader module.
Copyright   : (c) Maurizio Dusi, 2024
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module defines functions for testing functions within the Reader module.
-}

module TestReader
    ( readerTests
    ) where

import           Reader           ( Env (..) )

import           Test.Tasty       ( TestTree, testGroup )
import           Test.Tasty.HUnit ( testCase, (@?=) )


readerTests :: TestTree
readerTests =
    testGroup "Reader tests"
        [ testCase "environment initialization" $ do
            let env = Env { hostName = "localhost"
                          , port = 22
                          , user = "testuser"
                          , password = "testpass"
                          , knownHosts = "/path/to/known_hosts"
                          , transferFrom = "/path/to/source"
                          , transferTo = "/path/to/destination"
                          , transferExtensions = []
                          , archiveTo = Nothing
                          , date = 0
                          , noOp = False
                          }

            hostName env @?= "localhost"
            port env @?= 22
            user env @?= "testuser"
            password env @?= "testpass"
            knownHosts env @?= "/path/to/known_hosts"
            transferFrom env @?= "/path/to/source"
            transferTo env @?= "/path/to/destination"
            transferExtensions env @?= []
            archiveTo env @?= Nothing
            date env @?= 0
            noOp env @?= False
        ]

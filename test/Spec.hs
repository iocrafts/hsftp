{-|
Module      : Main
Description : Main entry point for the test suite for SFTP actions.
Copyright   : (c) Maurizio Dusi, 2024
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module defines the main entry point for the test suite, including
setup and teardown operations for testing SFTP commands such as downloading
and uploading files.
-}

module Main
    ( main
    ) where

import           Test.Tasty   ( TestTree, defaultMain, testGroup )

import           TestCommands ( sftpCommandsTests )

import           TestUtil     ( utilTests )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [sftpCommandsTests, utilTests]

{-|
Module      : Main
Description : Main entry point for the test suite for SFTP actions.
Copyright   : (c) IOcrafts, 2024-present
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

import           TestCommands ( sftpDownloadTests, sftpUploadTests )

import           TestReader   ( readerTests )

import           TestUtil     ( utilTests )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [sftpDownloadTests, sftpUploadTests, utilTests, readerTests]

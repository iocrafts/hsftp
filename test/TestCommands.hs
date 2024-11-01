{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : TestCommands
Description : Test functions for SFTP actions.
Copyright   : (c) Maurizio Dusi, 2024
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module defines functions for testing SFTP actions such as downloading
and uploading files.
-}

module TestCommands
    ( sftpDownloadTests
    , sftpUploadTests
    ) where

import           Commands                   ( download, upload )

import           Control.Monad              ( filterM )
import           Control.Monad.Reader

import           Data.Maybe                 ( fromJust )

import           Reader                     ( Env (..) )

import           System.Directory           ( createDirectoryIfMissing,
                                              doesFileExist, listDirectory,
                                              removeDirectoryRecursive )
import           System.FilePath            ( (</>) )
import           System.FilePath.ByteString ( encodeFilePath, isExtensionOf )
import           System.IO                  ( hClose, openTempFile )

import           Test.Tasty                 ( TestTree, testGroup,
                                              withResource )
import           Test.Tasty.HUnit


-- | Represents the test environment configuration.
data TestEnv
  = TestEnv { sftpHost            :: String
            , sftpPort            :: Int
            , sftpUser            :: String
            , sftpPass            :: String
            , repoLocalDir        :: String
            , sftpRemoteDir       :: String
            , sftpLocalBaseDir    :: String
            , sftpLocalUploadDir  :: String
            , sftpLocalArchiveDir :: String
            }
  deriving (Show)

uploadConfs :: TestEnv
uploadConfs = TestEnv { sftpHost = "0.0.0.0"
                      , sftpPort = 9988
                      , sftpUser = "demo"
                      , sftpPass = "demo"
                      , repoLocalDir = "./test/files/upload"
                      , sftpRemoteDir = "/upload"
                      , sftpLocalBaseDir = "/tmp/hsftp-u"
                      , sftpLocalUploadDir = "/tmp/hsftp-u/upload"
                      , sftpLocalArchiveDir = "/tmp/hsftp-u/archive"
                      }

downloadConfs :: TestEnv
downloadConfs = TestEnv { sftpHost = "0.0.0.0"
                        , sftpPort = 9988
                        , sftpUser = "demo"
                        , sftpPass = "demo"
                        , repoLocalDir = "./test/files/download"
                        , sftpRemoteDir = "/download"
                        , sftpLocalBaseDir = "/tmp/hsftp-d"
                        , sftpLocalUploadDir = "/tmp/hsftp-d/upload"
                        , sftpLocalArchiveDir = ""
                        }

-- | Acquires a resource and returns an 'Env' configuration.
--   The resource is acquired by creating a directory if it doesn't exist,
--   and opening a temporary file in the specified directory.
--   The 'Env' configuration includes details such as host name, port, user credentials,
--   and the path to the known hosts file.
acquireResource :: TestEnv -> IO Env
acquireResource teConfs = do
  let TestEnv {..} = teConfs
  createDirectoryIfMissing True sftpLocalBaseDir

  (knownHostsFile, hKnownHostsFile) <- openTempFile sftpLocalBaseDir "known_hosts"
  hClose hKnownHostsFile
  let env = Env { hostName = sftpHost
                , port = sftpPort
                , user = sftpUser
                , password = sftpPass
                , date = 0
                , archiveTo = Nothing
                , knownHosts = knownHostsFile
                , transferFrom = ""
                , transferTo = ""
                , transferExtensions = []
                }
  return env


-- | Release the resource by removing the local base directory and its contents.
--
-- This function takes a 'TestEnv' configuration and an 'Env' configuration.
-- It removes the local base directory and all its contents using the 'removeDirectoryRecursive' function.
releaseResource :: TestEnv -> Env -> IO ()
releaseResource teConfs _env = do
  removeDirectoryRecursive $ sftpLocalBaseDir teConfs


-- | This function defines the test tree for the SFTP upload command.
sftpUploadTests :: TestTree
sftpUploadTests =
  withResource (acquireResource uploadConfs) (releaseResource uploadConfs) $ \getResource ->
    testGroup "SFTP Upload tests" $
      let TestEnv {..} = uploadConfs
      in
      [ testCase "Filter by extension" $ do
          env <- getResource
          let extensions = ["log"]
              byExtension x = null extensions || or [extension `isExtensionOf` encodeFilePath x | extension <- extensions]
              env' = env { transferFrom = repoLocalDir
                         , transferTo = sftpRemoteDir </> "byext"
                         , transferExtensions = extensions
                         }

          numFiles <- runReaderT upload env'
          allFiles <- listDirectory repoLocalDir >>= filterM ( doesFileExist . (repoLocalDir </>) )
          let expectedFiles = filter byExtension allFiles
              expectedNumFiles = length expectedFiles
          numFiles @?= expectedNumFiles

      , testCase "Any extension" $ do
          env <- getResource
          let env' = env { transferFrom = repoLocalDir
                         , transferTo = sftpRemoteDir </> "anyext"
                         }

          numFiles <- runReaderT upload env'
          expectedFiles <- listDirectory repoLocalDir >>= filterM ( doesFileExist . (repoLocalDir </>) )
          numFiles @?= length expectedFiles

      {--
      , testCase "Upload and archive" $ do
          env <- getResource
          let env' = env { transferFrom = repoLocalDir
                         , transferTo = sftpRemoteDir </> "archive"
                         , archiveTo = Just sftpLocalArchiveDir
                         }

          numFiles <- runReaderT upload env'
          let archivedFile = fromJust (archiveTo env') </> takeFileName uploadedFile
          a <- doesFileExist archivedFile
          a @?= True
      --}
      ]


-- | This function defines the test tree for the SFTP download command.
sftpDownloadTests :: TestTree
sftpDownloadTests =
  withResource (acquireResource downloadConfs) (releaseResource downloadConfs) $ \getResource ->
    testGroup "SFTP Download tests" $
      let TestEnv {..} = downloadConfs
      in
      [ testCase "Filter by extension" $ do
          env <- getResource
          let testFolder = sftpLocalBaseDir </> "byext"
          createDirectoryIfMissing False testFolder

          let extensions = ["log"]
              env' = env { transferFrom = sftpRemoteDir
                         , transferTo = testFolder
                         , transferExtensions = extensions
                         }
              byExtension x = null extensions || or [extension `isExtensionOf` encodeFilePath x | extension <- extensions]

          numFiles <- runReaderT download env'
          allFiles <-  listDirectory repoLocalDir >>= filterM ( doesFileExist . (repoLocalDir </>) )
          let expectedFiles = filter byExtension allFiles
              expectedNumFiles = length expectedFiles
          numFiles @?= expectedNumFiles

      , testCase "Any extension" $ do
          env <- getResource
          let testFolder = sftpLocalBaseDir </> "anyext"
          createDirectoryIfMissing False testFolder

          let env' = env { transferFrom = sftpRemoteDir
                         , transferTo = testFolder
                         }
          numFiles <- runReaderT download env'
          expectedFiles <- listDirectory repoLocalDir >>= filterM ( doesFileExist . (repoLocalDir </>) )
          numFiles @?= length expectedFiles
      ]

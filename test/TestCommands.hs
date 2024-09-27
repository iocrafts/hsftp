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
    ( sftpCommandsTests
    ) where

import           Commands                   ( download, upload )

import           Control.Monad.Reader

import qualified Data.ByteString.Char8      as C
import           Data.Maybe                 ( fromJust )

import           Network.SSH.Client.LibSSH2 ( sftpListDir, sftpSendFile,
                                              withSFTPUser )

import           Reader                     ( Env (..) )

import           System.Directory           ( createDirectoryIfMissing,
                                              doesFileExist, getFileSize,
                                              listDirectory, removeDirectory,
                                              removeFile )
import           System.FilePath            ( takeFileName, (</>) )
import           System.IO                  ( hClose, openTempFile )

import           Test.Tasty                 ( TestTree, testGroup,
                                              withResource )
import           Test.Tasty.HUnit

-- Write your test cases here

-- | Represents the test environment configuration.
data TestEnv
  = TestEnv { sftpHost              :: String
            , sftpPort              :: Int
            , sftpUser              :: String
            , sftpPass              :: String
            , sftpLocalUploadDir    :: String
            , sftpRemoteUploadDir   :: String
            , sftpLocalDownloadDir  :: String
            , sftpRemoteDownloadDir :: String
            , sftpKnownHostsDir     :: String
            , sftpLocalArchiveDir   :: String
            }
  deriving (Show)

confs :: TestEnv
confs = TestEnv { sftpHost = "0.0.0.0"
                , sftpPort = 9988
                , sftpUser = "demo"
                , sftpPass = "demo"
                , sftpLocalUploadDir = "/tmp"
                , sftpRemoteUploadDir = "/upload"
                , sftpLocalDownloadDir = "/tmp/demo"
                , sftpRemoteDownloadDir = "/upload"
                , sftpKnownHostsDir = "/tmp"
                , sftpLocalArchiveDir = "/tmp/archive"
                }

-- | Acquires a resource and returns the file path of the acquired resource.
--   The resource is acquired by creating a directory if it doesn't exist,
--   and opening a temporary file in the specified directory.
--   The file path of the temporary file is returned.
acquireResource :: IO FilePath
acquireResource = do
  let TestEnv {..} = confs
  createDirectoryIfMissing True sftpLocalDownloadDir
  createDirectoryIfMissing True sftpLocalArchiveDir
  (knownHostsFile, hKnownHostsFile) <- openTempFile sftpKnownHostsDir "known_hosts"
  hClose hKnownHostsFile
  return knownHostsFile


-- | Release the resource by removing the known hosts file and the local download directory.
--
-- This function takes a 'FilePath' representing the path to the known hosts file. It removes the known hosts file
-- using 'removeFile' function. Then, it lists all the files in the local download directory using 'listDirectory'
-- function and removes each file using 'removeFile' function. Finally, it removes the local download directory
-- using 'removeDirectory' function.
releaseResource :: FilePath -> IO ()
releaseResource knownHostsFile = do
  let TestEnv {..} = confs
  removeFile knownHostsFile

  fs <- listDirectory sftpLocalDownloadDir
  mapM_ ( removeFile . (sftpLocalDownloadDir </>) ) fs >>
    removeDirectory sftpLocalDownloadDir

  as <- listDirectory sftpLocalArchiveDir
  mapM_ ( removeFile . (sftpLocalArchiveDir </>) ) as >>
    removeDirectory sftpLocalArchiveDir

-- | This function defines the test tree for the SFTP actions.
sftpCommandsTests :: TestTree
sftpCommandsTests =
    withResource acquireResource releaseResource $ \getResource ->
        testGroup "SFTP tests" $
          let TestEnv {..} = confs
              env = Env { hostName = sftpHost
                        , port = sftpPort
                        , user = sftpUser
                        , password = sftpPass
                        , date = 0
                        , archiveTo = Nothing
                        , knownHosts = ""
                        , transferFrom = ""
                        , transferTo = ""
                        , transferExtensions = []
                        }
          in
          [ testCase "SFTP upload" $ do
              knownHostsFile <- getResource
              (uploadedFile, hUploadedFile) <- openTempFile sftpLocalUploadDir "testfile.ulog"
              hClose hUploadedFile
              let env' = env { transferFrom = sftpLocalUploadDir
                             , transferTo = sftpRemoteUploadDir
                             , transferExtensions = ["ulog"]
                             , knownHosts = knownHostsFile
                             }
              runReaderT upload env'
              isUploaded <- withSFTPUser (knownHosts env') (user env') (password env') (hostName env') (port env') $ \sftp -> do
                allFiles <- sftpListDir sftp (transferTo env')
                return $ takeFileName uploadedFile `elem` map (C.unpack . fst) allFiles
              isUploaded @?= True
              removeFile uploadedFile

          , testCase "SFTP upload and archive" $ do
              knownHostsFile <- getResource
              (uploadedFile, hUploadedFile) <- openTempFile sftpLocalUploadDir "testfile.alog"
              hClose hUploadedFile
              let env' = env { transferFrom = sftpLocalUploadDir
                             , transferTo = sftpRemoteUploadDir
                             , transferExtensions = ["alog"]
                             , archiveTo = Just sftpLocalArchiveDir
                             , knownHosts = knownHostsFile
                             }
              runReaderT upload env'
              let archivedFile = fromJust (archiveTo env') </> takeFileName uploadedFile
              a <- doesFileExist archivedFile
              a @?= True

          , testCase "SFTP download" $ do
              knownHostsFile <- getResource
              (uploadedFile, hUploadedFile) <- openTempFile sftpLocalUploadDir "testfile.dlog"
              hClose hUploadedFile
              let env' = env { transferFrom = sftpRemoteDownloadDir
                             , transferTo = sftpLocalDownloadDir
                             , transferExtensions = ["dlog"]
                             , knownHosts = knownHostsFile
                             }
              _ <- withSFTPUser (knownHosts env') (user env') (password env') (hostName env') (port env') $ \sftp -> do
                    let src = uploadedFile
                        dst = transferFrom env' </> takeFileName uploadedFile
                    sftpSendFile sftp src dst 0o664

              runReaderT download env'
              let downloadedFile = transferTo env' </> takeFileName uploadedFile
              e <- doesFileExist downloadedFile
              s1 <- getFileSize uploadedFile
              s2 <- getFileSize downloadedFile
              e @?= True
              s1 @?= s2
              removeFile downloadedFile >> removeFile uploadedFile
          ]

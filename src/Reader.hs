{-|
Module      : Reader
Description : Holds environment variables.
Copyright   : (c) Maurizio Dusi, 2024
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module holds the environment variables used by the program.
-}

module Reader
    ( Env (..)
    , ReaderIO
    ) where

import           Control.Monad.Reader ( ReaderT )

import qualified Data.ByteString      as B

-- | Represents the environment configuration for the SFTP client.
data Env
  = Env { hostName           :: String
          -- ^ The hostname of the SFTP server.
        , port               :: Int
          -- ^ The port number to connect to.
        , user               :: String
          -- ^ The username for authentication.
        , password           :: String
          -- ^ The password for authentication.
        , knownHosts         :: FilePath
          -- ^ The path to the known hosts file.
        , transferFrom       :: FilePath
          -- ^ The source file path for transfer.
        , transferTo         :: FilePath
          -- ^ The destination file path for transfer.
        , transferExtensions :: [B.ByteString]
          -- ^ The list of file extensions to transfer.
        , archiveTo          :: Maybe FilePath
          -- ^ Optional path to archive transferred files.
        , date               :: Integer
          -- ^ The date for filtering files to transfer.
        , noOp               :: Bool
          -- ^ Whether or not to perform the actual transfer.
        }

type ReaderIO = ReaderT Env IO

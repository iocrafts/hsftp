{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : Hsftp.Options
Description : Holds the options for the hsftp utility.
Copyright   : (c) IOcrafts, 2024-present
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module holds the available options for the hsftp utility.
-}

module Hsftp.Options
    ( Direction (..)
    , Options (..)
    ) where

import           Data.Data ( Data, Typeable )

data Direction
  = Up
  | Down
  deriving (Data, Eq, Show)

-- | Represents the options for the program.
data Options
  = Options { conf       :: FilePath
              -- ^ Path to the configuration file.
            , fromDate   :: String
              -- ^ Filter files by date (see 'toDate' for details on supported formats).
            , extensions :: [String]
              -- ^ Filter files by extensions.
            , direction  :: Direction
              -- ^ Direction of the transfer.
            , src        :: FilePath
              -- ^ Transfer from this folder (folder must exist).
            , dst        :: FilePath
              -- ^ Transfer to this folder (folder must exist).
            , archive    :: Maybe FilePath
              -- ^ Archive into this folder after successful upload (folder must exist).
            , verbose    :: Int
              -- ^ Verbose level.
            , dryRun     :: Bool
              -- ^ Do a dry-run (no-op) transfer.
            , others     :: [FilePath]
              -- ^ List of files and/or folders.
            }
  deriving stock (Data, Show, Typeable)
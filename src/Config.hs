{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Config
Description : Process the YAML configuration file.
Copyright   : (c) IOcrafts, 2024-present
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module parses a YAML file with configuration options.

__Example of conf.yaml:__

@
    remote:
        hostname: sftp.domain.com
        port: 22
        username: username
        password: password
        known_hosts: \/home\/user\/.ssh\/known_hosts
@

-}

module Config
    ( Config (..)
    , mkConfig
    ) where

import           Control.Monad ( MonadPlus (mzero) )

import           Data.Aeson    ( FromJSON (parseJSON), (.!=), (.:) )
import qualified Data.Yaml     as Y


-- | Represents the configuration settings for the application.
data Config
  = Config { -- | The host address of the server.
             configHost       :: String
             -- | The port number to connect to.
           , configPort       :: Int
             -- | The username for authentication.
           , configUser       :: String
             -- | The password for authentication.
           , configPassword   :: String
             -- | The file path to the known hosts file.
           , configKnownHosts :: FilePath
           }
  deriving (Show)

-- | Represents a YAML configuration with a remote value.
newtype YamlConfig
  = YamlConfig { yamlRemote :: Remote }
  deriving (Show)

-- | Represents a remote SFTP configuration.
data Remote
  = Remote { remoteHost       :: String
             -- ^ SFTP site
           , remotePort       :: Int
             -- ^ SFTP port
           , remoteUser       :: String
             -- ^ SFTP username
           , remotePassword   :: String
             -- ^ SFTP password
           , remoteKnownHosts :: FilePath
             -- ^ Path to the known_hosts file
           }
  deriving (Show)

-- | Create a 'Config' from a 'YamlConfig'.
--
-- This function takes a 'YamlConfig' and extracts the necessary fields to create a 'Config' object.
-- It returns an 'IO' action that produces the resulting 'Config'.
mkConfig :: YamlConfig -> IO Config
mkConfig YamlConfig{..} = do
  let Remote {..} = yamlRemote
  return $
    Config { configHost = remoteHost
           , configPort = remotePort
           , configUser = remoteUser
           , configPassword = remotePassword
           , configKnownHosts = remoteKnownHosts
           }


-- | Parses a JSON object into a 'YamlConfig' value.
instance FromJSON YamlConfig where
  parseJSON (Y.Object v) =
    YamlConfig  <$> v .:   "remote"
  parseJSON _ = mzero

-- | Parses a JSON object into a 'Remote' data type.
instance FromJSON Remote where
  parseJSON (Y.Object v) =
    Remote  <$> v .:   "hostname"
            <*> v .:   "port"         .!= 22
            <*> v .:   "username"
            <*> v .:   "password"
            <*> v .:   "known_hosts"
  parseJSON _ = mzero

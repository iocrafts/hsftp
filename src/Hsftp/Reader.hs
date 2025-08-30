{-|
Module      : Hsftp.Reader
Description : Monad for keeping shared state.
Copyright   : (c) IOcrafts, 2024-present
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module provides the Reader monad for keeping shared state.

-}


module Hsftp.Reader
    ( Env (..)
    , ReaderIO
    ) where

import           Control.Monad.Reader

data Env = Env
    { hostName           :: String
    , port               :: Int
    , knownHosts         :: String
    , user               :: String
    , password           :: String
    , transferFrom       :: String
    , transferTo         :: String
    , transferExtensions :: [String]
    , archiveTo          :: Maybe String
    , date               :: Integer
    , noOp               :: Bool
    } deriving (Show)


type ReaderIO a = ReaderT Env IO a
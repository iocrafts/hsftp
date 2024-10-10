{-|
Module      : Util
Description : Collections of utility functions.
Copyright   : (c) Maurizio Dusi, 2024
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module provides utility functions for file, date and time manipulation.

-}


module Util
    ( createFile
    , toDate
    , toEpoch
    ) where

import           Control.Monad    ( unless )

import           Data.Time

import           System.Directory ( doesFileExist )

{-|
  Convert a string to seconds since Epoch.
  The input string should be in the format %F %R %Z (YYYY-MM-DD HH-mm and abbreviated time zone name).
  If the parsing fails, it defaults to the beginning of Epoch (i.e., zero).

  Example usage:

  >>> toDate "2022-01-01 12:00 UTC"
  2022-01-01 12:00:00 UTC
-}
toDate :: String -> UTCTime
toDate d =  case parseTimeM True defaultTimeLocale "%F %R %Z" d of
              Just x -> x
              Nothing -> UTCTime (fromGregorian 1970 01 01) (secondsToDiffTime 0)

{-|
  Convert a 'UTCTime' value to seconds since Epoch.

  Example usage:

   >>> toEpoch (UTCTime (fromGregorian 2022 01 01) (secondsToDiffTime 0))
  1640995200
-}
toEpoch :: UTCTime -> Integer
toEpoch d = read $ formatTime defaultTimeLocale "%s" d

{-|
  Create a file if it does not exist.

  Example usage:

  >>> createFile "test.txt"
-}
createFile :: FilePath -> IO ()
createFile cfile = do
  fileExists <- doesFileExist cfile
  unless fileExists $ writeFile cfile ""

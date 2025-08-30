{-|
Module      : Hsftp.CmdOptions
Description : Command-line options.
Copyright   : (c) IOcrafts, 2024-present
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module holds the command-line options accepted by executable.
-}

module Hsftp.CmdOptions
    ( options
    ) where

import           Data.Version           ( showVersion )

import           Hsftp.Options          ( Direction (..), Options (..) )

import           Paths_hsftp            ( version )

import           System.Console.CmdArgs ( CmdArgs, Mode, args, cmdArgsMode,
                                          enum, explicit, groupname, help, name,
                                          program, summary, typ, (&=) )


-- | Defines the command line options for the `hsftp` program.
options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
          {     conf = ""         &= typ "FILE"  &= help "Load conf from file"
          ,     fromDate = ""     &= typ "DATE" &= help "Filter files by date (YYYY-MM-DD HH:MM UTC|PST|...)" &= explicit &= name "from-date"
          ,     extensions = []   &= help "Filter files by extensions"
          ,     direction = enum [Up &= help "upload", Down &= help "download"]
          ,     src = ""          &= typ "DIR" &= help "Folder to transfer from" &= explicit &= name "transfer-from"
          ,     dst = ""          &= typ "DIR" &= help "Folder to transfer to" &= explicit &= name "transfer-to"
          ,     archive = Nothing &= typ "DIR" &= help "Folder to archive to after upload" &= explicit &= name "archive-to"
          ,     verbose = 0       &= groupname "\nMiscellaneous" &= help "Verbose level: 1, 2 or 3" &= explicit &= name "verbose"
          ,     dryRun = False    &= help "Do a dry-run (\"No-op\") transfer." &= explicit &= name "dry-run" &= name "n"
          ,     others = []       &= args
          } &= summary ("Hsftp " <> showVersion version <> ". Usage: hsftp OPTION") &= program "hsftp"
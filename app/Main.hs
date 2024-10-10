module Main
    ( main
    ) where

import           CmdOptions             ( options )

import           Commands               ( download, upload )

import           Config

import           Control.Monad          ( when )
import           Control.Monad.Reader

import qualified Data.ByteString.Char8  as C
import qualified Data.Yaml              as Y

import           Options                ( Direction (..), Options (..) )

import           Reader                 ( Env (..) )

import           System.Console.CmdArgs ( cmdArgsRun )

import           Util                   ( createFile, toDate, toEpoch )


-- | Loads the environment configuration from a file.
--
-- The function reads the configuration file specified by the given file path,
-- decodes it using the `decodeFileEither` function from the `yaml` library,
-- and constructs an `Env` value based on the decoded configuration.
--
-- If the configuration file cannot be parsed, an error is thrown with a
-- pretty-printed parse exception.
--
-- Returns the constructed `Env` value.
loadEnv :: FilePath -> IO Env
loadEnv cfile = do
    config <- Y.decodeFileEither cfile
    Config {..} <- case config of
        Left e      -> error $ Y.prettyPrintParseException e
        Right yconf -> mkConfig yconf
    createFile configKnownHosts
    return Env  { hostName = configHost
                , port = configPort
                , knownHosts = configKnownHosts
                , user = configUser
                , password = configPassword
                , transferFrom = ""
                , transferTo = ""
                , transferExtensions = []
                , archiveTo = Nothing
                , date = 0
                }

main :: IO ()
main = do
    Options{..} <- cmdArgsRun options
    env <- loadEnv conf
    let date = toEpoch . toDate $ fromDate
        env' = env  { date = date
                    , transferFrom = src
                    , transferTo = dst
                    , transferExtensions = map C.pack extensions
                    , archiveTo = archive
                    }

    when (direction == Down) $ do
        runReaderT download env'

    when (direction == Up) $ do
        runReaderT upload env'

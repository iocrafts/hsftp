{-|
Module      : Commands
Description : Supported commands.
Copyright   : (c) Maurizio Dusi, 2024
License     : BSD
Maintainer  : Maurizio Dusi
Stability   : stable
Portability : POSIX

This module holds a collection of supported commands.
-}


module Commands
    ( download
    , upload
    ) where

import           Control.Monad                      ( filterM )
import           Control.Monad.Reader

import qualified Data.ByteString.Char8              as C

import           Network.SSH.Client.LibSSH2
import           Network.SSH.Client.LibSSH2.Foreign ( SftpAttributes (..) )

import           Reader                             ( Env (..), ReaderIO )

import           System.Directory                   ( copyFileWithMetadata,
                                                      getModificationTime,
                                                      listDirectory,
                                                      removeFile )
import           System.FilePath                    ( (</>) )
import           System.FilePath.ByteString         ( encodeFilePath,
                                                      isExtensionOf )

import           Util                               ( toEpoch )


{-|
  Download files from a remote server using SFTP.
  Both remote and local folders must exist.
-}
download :: ReaderIO ()
download = do
    Env{..} <- ask

    liftIO $ withSFTPUser knownHosts user password hostName port $ \sftp -> do
        allFiles <- sftpListDir sftp transferFrom
        let files = filter (\x -> (toInteger . saMtime . snd ) x >= date &&
                        or [extension `isExtensionOf` fst x | extension <- transferExtensions]) allFiles
            getFile f = do
                let f' = C.unpack f
                    src = transferFrom </> f'
                    dst = transferTo </> f'
                sftpReceiveFile sftp dst src
        mapM_ (getFile . fst) files

{-|
  Upload files to a remote server using SFTP.
  Both remote and local folders must exist.
-}
upload :: ReaderIO ()
upload = do
    Env{..} <- ask

    liftIO $ withSFTPUser knownHosts user password hostName port $ \sftp -> do
        let byExtension x = or [extension `isExtensionOf` encodeFilePath x | extension <- transferExtensions]
            byDate = fmap ( (>= date) . toEpoch ) . getModificationTime
        allFiles <- listDirectory transferFrom >>= filterM ( byDate . (transferFrom </>) )
        let files = filter byExtension allFiles
            putFile f = do
                let src = transferFrom </> f
                    dst = transferTo </> f
                sftpSendFile sftp src dst 0o664
            archiveFile f = case archiveTo of
                Nothing -> return ()
                Just d -> do
                    let src = transferFrom </> f
                        dst = d </> f
                    copyFileWithMetadata src dst >> removeFile src
        mapM_ (\x -> putFile x >> archiveFile x) files

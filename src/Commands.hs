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

import           Data.Bits                          ( (.&.) )
import qualified Data.ByteString.Char8              as C

import           Network.SSH.Client.LibSSH2
import           Network.SSH.Client.LibSSH2.Foreign ( SftpAttributes (..) )

import           Reader                             ( Env (..), ReaderIO )

import           System.Directory                   ( copyFileWithMetadata,
                                                      doesFileExist,
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
  The function returns the number of files downloaded.
-}
download :: ReaderIO Int
download = do
    Env{..} <- ask

    liftIO $ withSFTPUser knownHosts user password hostName port $ \sftp -> do
        allFiles <- sftpListDir sftp transferFrom
        let byDate x = (toInteger . saMtime . snd) x >= date
            byExtension x = null transferExtensions || or [extension `isExtensionOf` fst x | extension <- transferExtensions]
            isFile = (== 0o100000) . (.&. 0o170000) . saPermissions . snd
            files = filter (\x -> byDate x && byExtension x && isFile x) allFiles
            getFile f = do
                let f' = C.unpack f
                    src = transferFrom </> f'
                    dst = transferTo </> f'
                sftpReceiveFile sftp dst src
        mapM_ (getFile . fst) files
        return $ length files

{-|
  Upload files to a remote server using SFTP.
  Both remote and local folders must exist.
  The function returns the number of files uploaded.
-}
upload :: ReaderIO Int
upload = do
    Env{..} <- ask
    let byExtension x = null transferExtensions || or [extension `isExtensionOf` encodeFilePath x | extension <- transferExtensions]
        byDate = fmap ( (>= date) . toEpoch ) . getModificationTime
    allFiles <- liftIO $ listDirectory transferFrom >>=
                    filterM ( doesFileExist . (transferFrom </>) ) >>=
                    filterM ( byDate . (transferFrom </>) )
    let files = filter byExtension allFiles

    liftIO $ withSFTPUser knownHosts user password hostName port $ \sftp -> do
        let putFile f = do
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
        return $ length files

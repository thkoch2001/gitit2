module Network.Gitit2.Helper (
  allPageFiles,
  getConfig,
  getRawContents,
  isPageFile,
  isDiscussPageFile,
  pathForPage,
  pageForPath
  ) where

import Control.Applicative ((<$>))
import Control.Exception (handle, throw)
import Data.ByteString.Lazy (ByteString)
import Data.FileStore (FileStoreError(NotFound), retrieve, RevisionId)
import Network.Gitit2.Foundation (config, filestore, GititConfig, GH, HasGitit, page_extension)
import Network.Gitit2.Page (pathForPageP, pageForPathP, Page)
import System.FilePath (takeExtension)
import Yesod (getYesod, liftIO)
import Data.FileStore (index)
import Control.Monad (filterM)

getConfig :: GH master GititConfig
getConfig = config <$> getYesod

pathForPage :: Page -> GH master FilePath
pathForPage p = do
  conf <- getConfig
  return $ pathForPageP (page_extension conf) p

pageForPath :: FilePath -> GH master Page
pageForPath fp = do
  conf <- getConfig
  return $ pageForPathP (page_extension conf) fp

getRawContents :: HasGitit master
               => FilePath
               -> Maybe RevisionId
               -> GH master (Maybe ByteString)
getRawContents path rev = do
  fs <- filestore <$> getYesod
  liftIO $ handle (\e -> if e == NotFound then return Nothing else throw e)
         $ Just <$> retrieve fs path rev

isPageFile :: FilePath -> GH master Bool
isPageFile f = do
  conf <- getConfig
  return $ takeExtension f == page_extension conf

isDiscussPageFile :: FilePath -> GH master Bool
isDiscussPageFile ('@':xs) = isPageFile xs
isDiscussPageFile _ = return False

allPageFiles :: GH master [FilePath]
allPageFiles = do
  fs <- filestore <$> getYesod
  liftIO (index fs) >>= filterM isPageFile

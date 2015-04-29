module Network.Gitit2.Cache
       where

import           Blaze.ByteString.Builder (toLazyByteString)
import           Control.Applicative ((<$>))
import           Control.Monad (filterM, unless, when)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.UTF8 as BSU
import           Data.Time (diffUTCTime, getCurrentTime)
import           Network.Gitit2.Foundation (GH, cache_dir, feed_minutes, use_cache)
import           Network.Gitit2.Helper (getConfig)
import           Network.HTTP (urlDecode, urlEncode)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                   getDirectoryContents, getModificationTime, removeDirectoryRecursive)
import           System.FilePath ((</>), takeDirectory)
import           Yesod (Content(ContentBuilder), liftIO, sendFile, toTypedContent, ToTypedContent, TypedContent(TypedContent))

tryCache :: FilePath -> GH master ()
tryCache path = do
  conf <- getConfig
  when (use_cache conf) $
     do
       let fullpath = cache_dir conf </> path
       exists <- liftIO $ doesDirectoryExist fullpath
       when exists $
          do
            files <- liftIO $ getDirectoryContents fullpath >>=
                               filterM (doesFileExist . (fullpath </>))
            case files of
                 (x:_) -> do
                    let ct = BSU.fromString $ urlDecode x
                    sendFile ct $ fullpath </> x
                 _     -> return ()

caching :: ToTypedContent a
        => FilePath -> GH master a -> GH master a
caching path handler = do
  conf <- getConfig
  if use_cache conf
     then do
       result <- handler
       cacheContent path $ toTypedContent result
       return result
     else handler

cacheContent :: FilePath -> TypedContent -> GH master ()
cacheContent path (TypedContent ct content) = do
  conf <- getConfig
  when (use_cache conf) $
       case content of
            ContentBuilder builder _ -> liftIO $ do
              let fullpath = cache_dir conf </> path </> urlEncode (BSU.toString ct)
              createDirectoryIfMissing True $ takeDirectory fullpath
              B.writeFile fullpath $ toLazyByteString builder
            _ -> liftIO $
              -- TODO replace w logging
              putStrLn $ "Can't cache " ++ path

expireCache :: FilePath -> GH master ()
expireCache path = do
  conf <- getConfig
  expireFeed (feed_minutes conf) (path </> "_feed")
  expireFeed (feed_minutes conf) "_feed"
  expireCategories
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> path
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ removeDirectoryRecursive fullpath


expireCategories :: GH master ()
expireCategories = do
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> "_categories"
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ removeDirectoryRecursive fullpath

-- | Expire the cached feed unless it is younger than 'minutes' old.
expireFeed :: Integer -> FilePath -> GH master ()
expireFeed minutes path = do
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> path
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ do
      seconds <- getModificationTime fullpath
      seconds' <- getCurrentTime
      unless (diffUTCTime seconds' seconds < realToFrac (minutes * 60))
        $ removeDirectoryRecursive fullpath

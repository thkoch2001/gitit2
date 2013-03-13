{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies,
    OverloadedStrings #-}
import Network.Gitit2
import Network.Socket hiding (Debug)
import Yesod
import Yesod.Default.Handlers (getFaviconR, getRobotsR)
import Yesod.Static
import Network.Wai.Handler.Warp
import Data.FileStore
import Data.Yaml
import Control.Applicative
import Control.Monad (when)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import System.IO
import System.Exit
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (catch)
import Control.Exception (catch, SomeException)
-- TODO only for samplePlugin
import Data.Generics
import Text.Pandoc.Definition

data Master = Master { getGitit :: Gitit, maxUploadSize :: Int }
mkYesod "Master" [parseRoutes|
/gitit SubsiteR Gitit getGitit
/   RootR GET
/favicon.ico FaviconR GET
/robots.txt RobotsR GET
|]

instance Yesod Master where
  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent $ do
      contents
    mmsg <- getMessage
    hamletToRepHtml [hamlet|
        $doctype 5
        <html>
          <head>
             <title>#{title}
             ^{headTags}
          <body>
             $maybe msg  <- mmsg
               <p.message>#{msg}
             ^{bodyTags}
        |]
  maximumContentLength x _ = fromIntegral $ maxUploadSize x

instance RenderMessage Master FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage Master GititMessage where
    renderMessage x = renderMessage (getGitit x)

instance HasGitit Master where
  maybeUser = return $ Just $ GititUser "Dummy" "dumb@dumber.org"
  requireUser = return $ GititUser "Dummy" "dumb@dumber.org"
  makePage = makeDefaultPage
  getPlugins = return [] -- [samplePlugin]

-- | A get request for / will redirect to the gitit subsite's home page.
getRootR :: Handler RepHtml
getRootR = do
    redirect $ SubsiteR $ HomeR

-- | Ready collection of common mime types. (Copied from
-- Happstack.Server.HTTP.FileServe.)
mimeTypes :: M.Map String ContentType
mimeTypes = M.fromList
        [("xml","application/xml")
        ,("xsl","application/xml")
        ,("js","text/javascript; charset=UTF-8")
        ,("html","text/html; charset=UTF-8")
        ,("htm","text/html; charset=UTF-8")
        ,("css","text/css; charset=UTF-8")
        ,("gif","image/gif")
        ,("jpg","image/jpeg")
        ,("png","image/png")
        ,("txt","text/plain; charset=UTF-8")
        ,("doc","application/msword")
        ,("exe","application/octet-stream")
        ,("pdf","application/pdf")
        ,("zip","application/zip")
        ,("gz","application/x-gzip")
        ,("ps","application/postscript")
        ,("rtf","application/rtf")
        ,("wav","application/x-wav")
        ,("hs","text/plain; charset=UTF-8")]

data Conf = Conf { cfg_port             :: Int
                 , cfg_listen_address   :: String
                 , cfg_repository_path  :: FilePath
                 , cfg_repository_type  :: Text
                 , cfg_page_extension   :: FilePath
                 , cfg_default_format   :: Text
                 , cfg_static_dir       :: FilePath
                 , cfg_mime_types_file  :: Maybe FilePath
                 , cfg_use_mathjax      :: Bool
                 , cfg_feed_days        :: Integer
                 , cfg_feed_minutes     :: Integer
                 , cfg_pandoc_user_data :: Maybe FilePath
                 , cfg_use_cache        :: Bool
                 , cfg_cache_dir        :: FilePath
                 , cfg_front_page       :: Text
                 , cfg_help_page        :: Text
                 , cfg_max_upload_size  :: String
                 , cfg_latex_engine     :: Maybe FilePath
                 }

-- | Read a file associating mime types with extensions, and return a
-- map from extensions to types. Each line of the file consists of a
-- mime type, followed by space, followed by a list of zero or more
-- extensions, separated by spaces. Example: text/plain txt text
readMimeTypesFile :: FilePath -> IO (M.Map String ContentType)
readMimeTypesFile f = catch
  ((foldr go M.empty . map words . lines) `fmap` readFile f)
  handleMimeTypesFileNotFound
     where go []     m = m  -- skip blank lines
           go (x:xs) m = foldr (\ext -> M.insert ext $ B.pack x) m xs
           handleMimeTypesFileNotFound :: SomeException -> IO (M.Map String ContentType)
           handleMimeTypesFileNotFound e = do
             warn $ "Could not parse mime types file.\n" ++ show e
             return mimeTypes

parseConfig :: Object -> Parser Conf
parseConfig o = Conf
  <$> o .:? "port" .!= 3000
  <*> o .:? "listen_address" .!= "0.0.0.0"
  <*> o .:? "repository_path" .!= "wikidata"
  <*> o .:? "repository_type" .!= "git"
  <*> o .:? "page_extension" .!= ".page"
  <*> o .:? "default_format" .!= "markdown"
  <*> o .:? "static_dir" .!= "static"
  <*> o .:? "mime_types_file"
  <*> o .:? "use_mathjax" .!= False
  <*> o .:? "feed_days" .!= 14
  <*> o .:? "feed_minutes" .!= 15
  <*> o .:? "pandoc_user_data"
  <*> o .:? "use_cache" .!= False
  <*> o .:? "cache_dir" .!= "cache"
  <*> o .:? "front_page" .!= "Front Page"
  <*> o .:? "help_page" .!= "Help"
  <*> o .:? "max_upload_size" .!= "1M"
  <*> o .:? "latex_engine"

readNumber :: String -> Maybe Int
readNumber x = case reads x of
                    ((n,""):_) -> Just n
                    _          -> Nothing

readSize :: String -> Maybe Int
readSize x =
  case reverse x of
       ('K':xs) -> (* 1000) <$> readNumber (reverse xs)
       ('M':xs) -> (* 1000000) <$> readNumber (reverse xs)
       ('G':xs) -> (* 1000000000) <$> readNumber (reverse xs)
       _        -> readNumber x

err :: Int -> String -> IO a
err code msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure code
  return undefined

warn :: String -> IO ()
warn msg = hPutStrLn stderr msg


-- TODO test
samplePlugin :: Plugin Master
samplePlugin = Plugin $ \wp -> do
 let spToUnderscore Space = return $ Str "_"
     spToUnderscore x     = return x
 newContent <- everywhereM (mkM spToUnderscore) $ wpContent wp
 return wp{ wpContent = newContent }

main :: IO ()
main = do
  res <- decodeEither `fmap` B.readFile "config/settings.yaml"
  conf <- case res of
             Left e  -> err 3 $ "Error reading configuration file.\n" ++ e
             Right x -> parseMonad parseConfig x
  let repopath = cfg_repository_path conf
  fs <- case T.toLower (cfg_repository_type conf) of
             "git"       -> return $ gitFileStore repopath
             "darcs"     -> return $ darcsFileStore repopath
             "mercurial" -> return $ mercurialFileStore repopath
             x           -> err 13 $ "Unknown repository type: " ++ T.unpack x
  st <- staticDevel $ cfg_static_dir conf
  mimes <- case cfg_mime_types_file conf of
                Nothing -> return mimeTypes
                Just f  -> readMimeTypesFile f

  -- open the requested interface
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  device <- inet_addr $ cfg_listen_address conf
  bindSocket sock $ SockAddrInet (toEnum (cfg_port conf)) device
  listen sock 10

  format <- case readPageFormat (cfg_default_format conf) of
                  Just f  -> return f
                  Nothing -> err 11 $ "Unknown default format: " ++
                                   T.unpack (cfg_default_format conf)
  maxsize <- case readSize (cfg_max_upload_size conf) of
                  Just s  -> return s
                  Nothing -> err 17 $ "Could not read size: " ++
                                      cfg_max_upload_size conf

  -- clear cache
  when (cfg_use_cache conf) $ do
    let cachedir = cfg_cache_dir conf
    exists <- doesDirectoryExist cachedir
    when exists $ removeDirectoryRecursive cachedir

  let settings = defaultSettings{ settingsPort = cfg_port conf }
  let runner = runSettingsSocket settings sock
  runner =<< toWaiApp
      (Master (Gitit{ config    = GititConfig{
                                    mime_types = mimes
                                  , default_format = format
                                  , repository_path = cfg_repository_path conf
                                  , page_extension = cfg_page_extension conf
                                  , use_mathjax = cfg_use_mathjax conf
                                  , feed_days  = cfg_feed_days conf
                                  , feed_minutes  = cfg_feed_minutes conf
                                  , pandoc_user_data = cfg_pandoc_user_data conf
                                  , use_cache = cfg_use_cache conf
                                  , cache_dir = cfg_cache_dir conf
                                  , front_page = cfg_front_page conf
                                  , help_page = cfg_help_page conf
                                  , latex_engine = cfg_latex_engine conf
                                  }
                    , filestore = fs
                    , getStatic = st
                    })
              maxsize)

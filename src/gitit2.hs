{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies,
    OverloadedStrings #-}
import Network.Gitit2
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Dummy
import Network.Wai.Handler.Warp
import Data.FileStore
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8
import System.FilePath ((<.>), (</>))
import Control.Monad (when, unless)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Data.Text (Text)
import qualified Data.Text as T
import Paths_gitit2 (getDataFileName)
import qualified Network.HTTP.Conduit as HC
-- TODO only for samplePlugin
import Data.Generics

import Config
import Error
import ArgParser

import Network.Gitit2.WikiPage (PageFormat(..), wpContent)

data Master = Master { settings :: FoundationSettings
                     , getGitit    :: Gitit
                     , maxUploadSize :: Int
                     , getStatic   :: Static
                     , httpManager :: HC.Manager
                     }
mkYesod "Master" [parseRoutes|
/static StaticR Static getStatic
/wiki SubsiteR Gitit getGitit
/auth AuthR Auth getAuth
/user UserR GET
/messages MessagesR GET
/ RootR GET
|]

getRootR :: Handler ()
getRootR = redirect $ SubsiteR HomeR

instance Yesod Master where
  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent $ do
      addScript $ staticR $ StaticRoute ["js","jquery-1.7.2.min.js"] []
      toWidget [julius|
        $.get("@{UserR}", {}, function(userpane, status) {
          $("#userpane").html(userpane);
        });
        $.get("@{MessagesR}", {}, function(messages, status) {
          $("#messages").html(messages);
        });
        |]
      contents
    withUrlRenderer [hamlet|
        $doctype 5
        <html>
          <head>
             <meta charset="UTF-8">
             <meta name="viewport" content="width=device-width,initial-scale=1">
             <title>#{title}
             ^{headTags}
          <body>
             ^{bodyTags}
        |]
  -- TODO: insert javascript calls to /user and /messages

  maximumContentLength x _ = Just $ fromIntegral $ maxUploadSize x

  -- needed for BrowserId - can we set it form config or request?
  approot = ApprootMaster $ appRoot . settings

instance YesodAuth Master where
  type AuthId Master = Text
  getAuthId = return . Just . credsIdent

  loginDest _ = RootR
  logoutDest _ = RootR

  authPlugins _ = [ authDummy ]

  maybeAuthId = lookupSession "_ID"

  authHttpManager = httpManager

  redirectToReferer _ = True

instance RenderMessage Master FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage Master GititMessage where
    renderMessage x = renderMessage (getGitit x)

instance HasGitit Master where
  maybeUser = do
    mbid <- lookupSession "_ID"
    case mbid of
         Nothing  -> return Nothing
         Just id' -> return $ Just $ GititUser
                        (T.unpack $ T.takeWhile (/='@') id')
                        (T.unpack id')
  requireUser = maybe (fail "login required") return =<< maybeUser
  isEditor user = do
    conf <- config <$> getYesod
    return $ case editors conf of
         Just emails ->  T.pack (gititUserEmail user) `elem` emails
         Nothing -> True
  requireEditor = do
    user <- requireUser
    editorUser <- isEditor user
    if editorUser
       then return user
       else fail "unauthorized"
  makePage = makeDefaultPage
  getPlugins = return [] -- [samplePlugin]
  staticR = StaticR

getUserR :: Handler Html
getUserR = do
  maid <- maybeAuthId
  withUrlRenderer [hamlet|
    $maybe aid <- maid
      <p><a href=@{AuthR LogoutR}>Logout #{aid}
    $nothing
      <a href=@{AuthR LoginR}>Login
    |]

getMessagesR :: Handler Html
getMessagesR = do
  mmsg <- getMessage
  withUrlRenderer [hamlet|
    $maybe msg  <- mmsg
      <p.message>#{msg}
    |]

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

-- TODO test
samplePlugin :: Plugin Master
samplePlugin = Plugin $ \wp -> do
 let spToUnderscore Space = return $ Str "_"
     spToUnderscore x     = return x
 newContent <- everywhereM (mkM spToUnderscore) $ wpContent wp
 return wp{ wpContent = newContent }

main :: IO ()
main = do
  conf <- readArgs "settings.yaml"
  let repopath = cfg_repository_path conf
  repoexists <- doesDirectoryExist repopath
  fs <- case T.toLower (cfg_repository_type conf) of
             "git"       -> return $ gitFileStore repopath
             "darcs"     -> return $ darcsFileStore repopath
             "mercurial" -> return $ mercurialFileStore repopath
             x           -> err 13 $ "Unknown repository type: " ++ T.unpack x

  st <- staticDevel $ cfg_static_dir conf
  maxsize <- case readSize (cfg_max_upload_size conf) of
                  Just s  -> return s
                  Nothing -> err 17 $ "Could not read size: " ++
                                      cfg_max_upload_size conf

  gconfig <- gititConfigFromConf conf

  -- clear cache
  when (use_cache gconfig) $ do
    let cachedir = cache_dir gconfig
    exists <- doesDirectoryExist cachedir
    when exists $ removeDirectoryRecursive cachedir

  unless repoexists $ initializeRepo gconfig fs

  man <- HC.newManager HC.tlsManagerSettings
  run (cfg_port conf)  =<< toWaiApp
      (Master (foundationSettingsFromConf conf)
              Gitit{ config = gconfig
                    , filestore = fs
                    }
              maxsize
              st
              man)

initializeRepo :: GititConfig -> FileStore -> IO ()
initializeRepo gconfig fs = do
  putStrLn $ "Creating initial repository in " ++ repository_path gconfig
  Data.FileStore.initialize fs
  templ <- runIOorExplode $ getDefaultTemplate "markdown"
  -- note: we convert this (markdown) to the default page format
  let converter f = do
        contents <- Paths_gitit2.getDataFileName f >>= UTF8.readFile
        let defOpts lhs = def{
               writerTemplate = Just templ
             , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
             , writerExtensions = if lhs
                                     then enableExtension Ext_literate_haskell
                                          $ writerExtensions def
                                     else writerExtensions def
             }
        let res = runPure $
               (readMarkdown def :: T.Text -> PandocPure Pandoc)
                 (T.pack contents)
               >>= (case default_format gconfig of
                          Markdown lhs -> writeMarkdown (defOpts lhs)
                          LaTeX    lhs -> writeLaTeX (defOpts lhs)
                          HTML     lhs -> writeHtml5String (defOpts lhs)
                          RST      lhs -> writeRST (defOpts lhs)
                          Textile  lhs -> writeTextile (defOpts lhs)
                          Org      lhs -> writeOrg (defOpts lhs))
        T.unpack <$> handleError res

  let fmt = takeWhile (/=' ') $ show $ default_format gconfig
  welcomecontents <- converter ("data" </> "FrontPage.page")
  helpcontentsInitial <- converter ("data" </> "Help.page")
  helpcontentsMarkup <- converter ("data" </> "markup" <.> fmt)
  usersguidecontents <- converter "README.markdown"
  -- include header in case user changes default format:
  let header = "---\nformat: " ++ fmt ++ "\n...\n\n"
  -- add front page, help page, and user's guide
  let auth = Author "Gitit" ""
  create fs (T.unpack (front_page gconfig) <.> "page") auth "Default front page"
    $ header ++ welcomecontents
  create fs (T.unpack (help_page gconfig) <.> "page") auth "Default help page"
    $ header ++ helpcontentsInitial ++ "\n\n" ++ helpcontentsMarkup
  create fs "Gitit User’s Guide.page" auth "User’s guide (README)"
    $ header ++ usersguidecontents

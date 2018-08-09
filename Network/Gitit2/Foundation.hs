{-# LANGUAGE FlexibleInstances, ViewPatterns #-}

module Network.Gitit2.Foundation where

import Data.FileStore (FileStore, RevisionId)
import qualified Data.Map as M
import Data.Text (Text)
import Text.Blaze.Html hiding (contents)
import Yesod hiding (MsgDelete)
import Yesod.Static

import Network.Gitit2.Page (Page)
import Network.Gitit2.WikiPage (PageFormat, WikiPage)

-- Create GititMessages.
mkMessage "Gitit" "messages" "en"

type GH master = HandlerT Gitit (HandlerT master IO)
type GW master = WidgetT Gitit (HandlerT master IO)

data Plugin master = Plugin {
       unPlugin :: WikiPage -> GH master WikiPage
       }

applyPlugin :: WikiPage -> Plugin master -> GH master WikiPage
applyPlugin wp pl = unPlugin pl wp

-- | The master site containing a Gitit subsite must be an instance
-- of this typeclass.
-- TODO: replace the user functions with isAuthorized from Yesod typeclass?
class (Yesod master, RenderMessage master FormMessage,
       RenderMessage master GititMessage) => HasGitit master where
  -- | Return user information, if user is logged in, or nothing.
  maybeUser   :: GH master (Maybe GititUser)
  -- | Return user information or redirect to login page.
  requireUser :: GH master GititUser
  -- | Determine whether a particular user is an editor.
  isEditor :: GititUser -> GH master Bool
  -- | Return user information or redirect to login page if no user.
  -- If user isn't an editor, show an unauthorized error.
  requireEditor :: HasGitit master => GH master GititUser
  -- | Gitit subsite page layout.
  makePage :: PageLayout -> WidgetT master IO () -> GH master Html
  -- | Plugins.
  getPlugins :: GH master [Plugin master]
  -- | Route for static content
  staticR :: Route Static -> Route master

-- | A Gitit wiki.  For an example of how a Gitit subsite
-- can be integrated into another Yesod app, see @src/gitit.hs@
-- in the package source.
data Gitit = Gitit{ config        :: GititConfig  -- ^ Wiki config options.
                  , filestore     :: FileStore    -- ^ Filestore with pages.
                  }

-- | Configuration for a gitit wiki.
data GititConfig = GititConfig{
       mime_types       :: M.Map String ContentType -- ^ Table of mime types
     , default_format   :: PageFormat               -- ^ Default format for wiki pages
     , repository_path  :: FilePath                 -- ^ Path to wiki
     , page_extension   :: FilePath                 -- ^ Extension for page files
     , static_path      :: FilePath                 -- ^ Path of static dir
     , use_mathjax      :: Bool                     -- ^ Link to mathjax script
     , feed_days        :: Integer                  -- ^ Days back for feed entries
     , feed_minutes     :: Integer                  -- ^ Minutes to cache feed before refresh
     , pandoc_user_data :: Maybe FilePath           -- ^ Pandoc userdata directory
     , use_cache        :: Bool                     -- ^ Cache pages and files
     , cache_dir        :: FilePath                 -- ^ Path to cache
     , front_page       :: Text                     -- ^ Front page of wiki
     , help_page        :: Text                     -- ^ Help page
     , latex_engine     :: Maybe FilePath           -- ^ LaTeX engine to use for PDF export
     , editors          :: Maybe [Text]             -- ^ Users allowed to actually edit
     }

-- | A user.
data GititUser = GititUser{ gititUserName  :: String
                          , gititUserEmail :: String
                          } deriving Show

-- | A tab in the page layout.
data Tab  = ViewTab
          | EditTab
          | HistoryTab
          | DiscussTab
          | DiffTab
          deriving (Eq, Show)

-- | Page layout.
data PageLayout = PageLayout{
    pgName           :: Maybe Page
  , pgPageTools      :: Bool
  , pgSiteNav        :: Bool
  , pgTabs           :: [Tab]
  , pgSelectedTab    :: Tab
  , pgCategories     :: [Text]
  }

-- | Default page layout.
pageLayout :: PageLayout
pageLayout = PageLayout{
    pgName           = Nothing
  , pgPageTools      = False
  , pgSiteNav        = True
  , pgTabs           = []
  , pgSelectedTab    = ViewTab
  , pgCategories     = []
  }

-- Create routes.

mkYesodSubData "Gitit" $(parseRoutesFile "Network/Gitit2/config/routes")

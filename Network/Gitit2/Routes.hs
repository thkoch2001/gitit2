{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE ViewPatterns          #-}
module Network.Gitit2.Routes where

import Data.FileStore (FileStore, RevisionId)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.Blaze.Html hiding (contents)
import Text.Pandoc (Inline, Block)
import Yesod hiding (MsgDelete)
import Yesod.Static
import Data.Typeable
import Data.Data
import GHC.Generics
import qualified Data.Aeson as ASON
import Text.Pandoc

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
     , toc_depth        :: Maybe Int                -- ^ Depth of table of contents
     , extended_toc     :: Bool                     -- ^ Toc extends over subpage
    , subpage_toc_in_content :: Bool                -- ^ Subpage extends to their Toc in the content
     }

-- | Path to a wiki page.  Page and page components can't begin with '_'.
data Page = Page [Text] deriving (Show, Read, Eq)

-- for now, we disallow @*@ and @?@ in page names, because git filestore
-- does not deal with them properly, and darcs filestore disallows them.
instance PathMultiPiece Page where
  toPathMultiPiece (Page x) = x
  fromPathMultiPiece []     = Nothing
  fromPathMultiPiece xs@(_:_) =
     if any (\x ->  "_" `T.isPrefixOf` x ||
                    "*" `T.isInfixOf` x ||
                    "?" `T.isInfixOf` x ||
                    ".." `T.isInfixOf` x ||
                    "/_" `T.isInfixOf` x) xs
                    then Nothing
                    else Just (Page xs)

pageToText :: Page -> Text
pageToText (Page xs) = T.intercalate "/" xs

textToPage :: Text -> Page
textToPage x = Page $ T.splitOn "/" x

instance ToMarkup Page where
  toMarkup = toMarkup . pageToText

instance ToMessage Page where
  toMessage = pageToText

instance ToMarkup (Maybe Page) where
  toMarkup (Just x) = toMarkup x
  toMarkup Nothing  = ""

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

-- | The Boolean is True for literate Haskell.
data PageFormat = Markdown Bool
                | RST Bool
                | LaTeX Bool
                | HTML Bool
                | Textile Bool
                | Org Bool
                deriving (Read, Show, Eq )

readPageFormat :: Text -> Maybe PageFormat
readPageFormat s =
  case T.toLower s' of
       "markdown"  -> Just $ Markdown lhs
       "textile"   -> Just $ Textile lhs
       "latex"     -> Just $ LaTeX lhs
       "html"      -> Just $ HTML lhs
       "rst"       -> Just $ RST lhs
       "org"       -> Just $ Org lhs
       _           -> Nothing
 where (s',rest) = T.break (=='+') s
       lhs = rest == "+lhs"
-- | Data type equivalent to Element where only sections and single links in paragraph have been kept
data GititToc = GititLink Int Text.Pandoc.Attr [Inline] Target
                --        lvl attributes       label    link
              | GititSec Int [Int] Text.Pandoc.Attr [Inline] [GititToc]
                --       lvl num   attributes       label    contents
                deriving (Eq, Read, Show, Typeable, Data, Generic)

instance ASON.FromJSON GititToc
instance ASON.ToJSON GititToc
data WikiPage = WikiPage {
    wpName        :: Text
  , wpFormat      :: PageFormat
  , wpTOC         :: Bool
  , wpLHS         :: Bool
  , wpTitle       :: [Inline]
  , wpCategories  :: [Text]
  , wpMetadata    :: M.Map Text Value
  , wpCacheable   :: Bool
  , wpContent     :: [Block]
  , wpTocHierarchy :: [GititToc]
} deriving (Show)

-- Create routes.
mkYesodSubData "Gitit" [parseRoutesNoCheck|
/ HomeR GET
/_help HelpR GET

/robots.txt GititRobotsR GET
/favicon.ico GititFaviconR GET
/_index IndexBaseR GET
/_index/*Page  IndexR GET
/_random RandomR GET
/_raw/*Page RawR GET
/_edit/*Page  EditR GET
/_revision/#RevisionId/*Page RevisionR GET
/_revert/#RevisionId/*Page RevertR GET
/_update/#RevisionId/*Page UpdateR POST
/_create/*Page CreateR POST
/_delete/*Page DeleteR GET POST
/_search SearchR POST
/_go GoR POST
/_upload UploadR GET POST
/_diff/#RevisionId/#RevisionId/*Page DiffR GET
/_history/#Int/*Page HistoryR GET
/_activity/#Int ActivityR GET
/_atom AtomSiteR GET
/_atom/*Page AtomPageR GET
/_export/#Text/*Page ExportR GET
/_expire/*Page ExpireR POST
/_expire ExpireHomeR POST
/_categories CategoriesR GET
/_category/#Text CategoryR GET
/_preview/*Page PreviewR POST
/*Page     ViewR GET
|]

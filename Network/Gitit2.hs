{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, FlexibleInstances,
             FlexibleContexts, ScopedTypeVariables, TupleSections,
             DeriveDataTypeable, DeriveGeneric,
             ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Gitit2 ( GititConfig (..)
                      , HtmlMathMethod (..)
                      , Page (..)
                      , PageFormat (..)
                      , readPageFormat
                      , HasGitit (..)
                      , Gitit (..)
                      , GititUser (..)
                      , GititMessage (..)
                      , Route (..)
                      , Tab (..)
                      , PageLayout (..)
                      , pageLayout
                      , makeDefaultPage
                      , Plugin (..)
                      , WikiPage (..)
                      ) where

import Prelude hiding (catch)
import qualified Data.Map as M
import Yesod hiding (MsgDelete)
import Yesod.Static
import Data.Ord (comparing)
import Data.FileStore as FS
import Data.Char (toLower)
import System.FilePath
import Data.List (inits, find, sortBy, isPrefixOf, sort, nub, intercalate)
import Text.Pandoc
import qualified Text.Pandoc.Writers.HTML as PWH (defaultWriterState, WriterState, inlineListToHtml, unordList)
import Text.Pandoc.Writers.RTF (writeRTFWithEmbeddedImages)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Shared (stringify, inDirectory, readDataFileUTF8, hierarchicalize, Element(..))
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Pandoc.Builder (toList, text)
import Control.Applicative
import Control.Monad (when, unless, filterM, mplus, foldM, void, liftM)
import Control.Monad.State(StateT, evalStateT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.Text.Encoding
import Data.ByteString.Lazy (ByteString, fromChunks, pack, hGetContents, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.UTF8 as BSU
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Text.Blaze.Html hiding (contents, text)
import Blaze.ByteString.Builder (toLazyByteString)
import Text.HTML.SanitizeXSS (sanitizeAttribute)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, isJust, isNothing)
import System.Random (randomRIO)
import System.IO (Handle, withFile, IOMode(..))
import System.IO.Error (isEOFError)
import Control.Exception (catch, throw, handle, try)
import Text.Highlighting.Kate
import Data.Time (getCurrentTime, addUTCTime)
import Yesod.AtomFeed
import Data.Yaml
import System.Directory
import Data.Time.Clock (diffUTCTime)
import Network.HTTP.Base (urlEncode, urlDecode)
import qualified Data.Set as Set

import Network.Gitit2.Routes
import qualified Data.Aeson as ASON
import qualified Text.Blaze.XHtml1.Transitional as H
import qualified Text.Blaze.XHtml1.Transitional.Attributes as A
import Network.URI ( unEscapeString )

-- This is defined in GHC 7.04+, but for compatibility we define it here.
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

instance HasGitit master => YesodSubDispatch Gitit (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesGitit)

data HtmlMathMethod = UseMathML | UseMathJax | UsePlainMath

getConfig :: GH master GititConfig
getConfig = config <$> getYesod

makeDefaultPage :: HasGitit master => PageLayout -> WidgetT master IO () -> GH master Html
makeDefaultPage layout content = do
  toMaster <- getRouteToParent
  let logoRoute = staticR $ StaticRoute ["img","logo.png"] []
  let feedRoute = staticR $ StaticRoute ["img","icons","feed.png"] []

  let searchRoute = toMaster SearchR
  let goRoute = toMaster GoR
  let tabClass :: Tab -> Text
      tabClass t = if t == pgSelectedTab layout then "selected" else ""
  let showTab t = t `elem` pgTabs layout
  exportFormats <- getExportFormats
  lift $ defaultLayout $ do
    addStylesheet $ staticR $ StaticRoute ["css","custom.css"] []
    addScript $ staticR $ StaticRoute ["js","custom.js"] []
    addScript $ staticR $ StaticRoute ["js","jquery-1.7.2.min.js"] []
    addScript $ staticR $ StaticRoute ["js","bootstrap.min.js"] []
    atomLink (toMaster AtomSiteR) "Atom feed for the wiki"
    toWidget $ [lucius|input.hidden { display: none; } |]
    $(whamletFile "template/custom/layout.hamlet")

-- HANDLERS and utility functions, not exported:

-- | Convert links with no URL to wikilinks.
convertWikiLinks :: Text -> Inline -> GH master Inline
convertWikiLinks prefix (Link ref ("", "")) = do
  toMaster <- getRouteToParent
  toUrl <- lift getUrlRender
  let route = ViewR $ textToPage $ T.append prefix $ T.pack $ stringify ref
  return $ Link ref (T.unpack $ toUrl $ toMaster route, "")
convertWikiLinks prefix (Image ref ("", "")) = do
  toMaster <- getRouteToParent
  toUrl <- lift getUrlRender
  let route = ViewR $ textToPage $ T.append prefix $ T.pack $ stringify ref
  return $ Image ref (T.unpack $ toUrl $ toMaster route, "")
convertWikiLinks _ x = return x

addWikiLinks :: Text -> Pandoc -> GH master Pandoc
addWikiLinks prefix = bottomUpM (convertWikiLinks prefix)

identifyParWikiLinks :: Block -> GH master Block
identifyParWikiLinks (Para [Link inlines target]) = do
  ident <- newIdent
  return $ Div (T.unpack ident,["subpage-link"],[]) [Para [Link inlines target]]
identifyParWikiLinks x = return x

addSubpageClass :: Pandoc -> GH master Pandoc
addSubpageClass = bottomUpM identifyParWikiLinks
sanitizePandoc :: Pandoc -> Pandoc
sanitizePandoc = bottomUp sanitizeBlock . bottomUp sanitizeInline
  where sanitizeBlock (RawBlock _ _) = Text.Pandoc.Null
        sanitizeBlock (CodeBlock (id',classes,attrs) x) =
          CodeBlock (id', classes, sanitizeAttrs attrs) x
        sanitizeBlock x = x
        sanitizeInline (RawInline _ _) = Str ""
        sanitizeInline (Code (id',classes,attrs) x) =
          Code (id', classes, sanitizeAttrs attrs) x
        sanitizeInline (Link lab (src,tit)) = Link lab (sanitizeURI src,tit)
        sanitizeInline (Image alt (src,tit)) = Image alt (sanitizeURI src,tit)
        sanitizeInline x = x
        sanitizeURI src = case sanitizeAttribute ("href", T.pack src) of
                               Just (_,z) -> T.unpack z
                               Nothing    -> ""
        sanitizeAttrs = mapMaybe sanitizeAttr
        sanitizeAttr (x,y) = case sanitizeAttribute (T.pack x, T.pack y) of
                                  Just (w,z) -> Just (T.unpack w, T.unpack z)
                                  Nothing    -> Nothing

pathForPage :: Page -> GH master FilePath
pathForPage p = do
  conf <- getConfig
  return $ T.unpack (toMessage p) <> page_extension conf

pathForToc :: Page -> GH master FilePath
pathForToc p = do
  conf <- getConfig
  return $ T.unpack (toMessage p) <> page_extension conf </> "toc"

pathForCategories :: Page -> GH master FilePath
pathForCategories p = do
  conf <- getConfig
  return $ T.unpack (toMessage p) <> page_extension conf </> "categories"

pathForFile :: Page -> GH master FilePath
pathForFile p = return $ T.unpack $ toMessage p

pageForPath :: FilePath -> GH master Page
pageForPath fp = do
  conf <- getConfig
  return $ textToPage $ T.pack $
    if takeExtension fp == page_extension conf
       then dropExtension fp
       else fp

isDiscussPage :: Page -> Bool
isDiscussPage (Page xs) = case reverse xs of
                               (x:_) -> "@" `T.isPrefixOf` x
                               _     -> False

discussPageFor :: Page -> Page
discussPageFor (Page xs)
  | isDiscussPage (Page xs) = Page xs
  | otherwise               = Page $ init xs ++ ["@" <> last xs]

discussedPage :: Page -> Page
discussedPage (Page xs)
  | isDiscussPage (Page xs) = Page $ init xs ++ [T.drop 1 $ last xs]
  | otherwise               = Page xs

isPageFile :: FilePath -> GH master Bool
isPageFile f = do
  conf <- getConfig
  return $ takeExtension f == page_extension conf

allPageFiles :: GH master [FilePath]
allPageFiles = do
  fs <- filestore <$> getYesod
  liftIO (index fs) >>= filterM isPageFile

isDiscussPageFile :: FilePath -> GH master Bool
isDiscussPageFile ('@':xs) = isPageFile xs
isDiscussPageFile _ = return False

isSourceFile :: FilePath -> GH master Bool
isSourceFile path' = do
  let langs = languagesByFilename $ takeFileName path'
  return $ not (null langs || takeExtension path' == ".svg")
                         -- allow svg to be served as image

getGititRobotsR :: GH m RepPlain
getGititRobotsR = do
  conf <- getConfig
  sendFile "text/plain" (static_path conf </> "robots.txt")

getGititFaviconR :: GH m ()
getGititFaviconR = do
  conf <- getConfig
  sendFile "image/x-icon" (static_path conf </> "favicon.ico")

getHomeR :: HasGitit master => GH master ()
getHomeR = do
  conf <- getConfig
  redirect $ ViewR $ textToPage $ front_page conf

getHelpR :: HasGitit master => GH master Html
getHelpR = do
  conf <- getConfig
  getViewR $ textToPage $ help_page conf

getRandomR :: HasGitit master => GH master Html
getRandomR = do
  fs <- filestore <$> getYesod
  files <- liftIO $ index fs
  pages <- mapM pageForPath =<< filterM (fmap not . isDiscussPageFile)
                            =<<filterM isPageFile files
  pagenum <- liftIO $ randomRIO (0, length pages - 1)
  let thepage = pages !! pagenum
  redirect $ ViewR thepage

getRawR :: HasGitit master => Page -> GH master RepPlain
getRawR page = do
  path <- pathForPage page
  mbcont <- getRawContents path Nothing
  case mbcont of
       Nothing       -> do
         path' <- pathForFile page
         mbcont' <- getRawContents path' Nothing
         case mbcont' of
              Nothing   -> notFound
              Just cont -> return $ RepPlain $ toContent cont
       Just cont -> return $ RepPlain $ toContent cont

getDeleteR :: HasGitit master => Page -> GH master Html
getDeleteR page = do
  requireUser
  fs <- filestore <$> getYesod
  path <- pathForPage page
  pageTest <- liftIO $ try $ latest fs path
  fileToDelete <- case pageTest of
                       Right _        -> return path
                       Left  FS.NotFound -> do
                         path' <- pathForFile page
                         fileTest <- liftIO $ try $ latest fs path'
                         case fileTest of
                              Right _     -> return path' -- a file
                              Left FS.NotFound  -> fail (show FS.NotFound)
                              Left e      -> fail (show e)
                       Left e        -> fail (show e)
  toMaster <- getRouteToParent
  makePage pageLayout{ pgName = Just page
                     , pgTabs = []
                     }
    [whamlet|
      <h1>#{page}</h1>
      <div #deleteform>
        <form method=post action=@{toMaster $ DeleteR page}>
          <p>_{MsgConfirmDelete page}
          <input type=text class=hidden name=fileToDelete value=#{fileToDelete}>
          <input type=submit value=_{MsgDelete}>
    |]

postDeleteR :: HasGitit master => Page -> GH master Html
postDeleteR page = do
  user <- requireUser
  fs <- filestore <$> getYesod
  mr <- getMessageRender
  fileToDelete <- lift $ runInputPost $ ireq textField "fileToDelete"
  liftIO $ FS.delete fs (T.unpack fileToDelete)
            (Author (gititUserName user) (gititUserEmail user))
            (T.unpack $ mr $ MsgDeleted page)
  setMessageI $ MsgDeleted page
  redirect HomeR

getViewR :: HasGitit master => Page -> GH master Html
getViewR page = do
  pathForFile page >>= tryCache
  view Nothing page

postPreviewR :: HasGitit master => Page -> GH master Html
postPreviewR page = do
  contents <- lift $ runInputPost $ ireq textField "contents"
  wikipage <- contentsToWikiPage page (fromStrict $ encodeUtf8 contents)
  pageToHtml wikipage
  -- undefined -- TODO: get raw contents and settings from post params
  -- return HTML for rendered page contents
  -- a javascript gizmo will display this in a modal or something
  -- factor out some of the code from view

getMimeType :: FilePath -> GH master ContentType
getMimeType fp = do
  mimeTypes <- mime_types <$> getConfig
  return $ fromMaybe "application/octet-stream"
         $ M.lookup (drop 1 $ takeExtension fp) mimeTypes

getRevisionR :: HasGitit master => RevisionId -> Page -> GH master Html
getRevisionR rev = view (Just rev)

-- | Retrieves toc, categories and content from cache or create them and cache them
wikifyAndCache :: HasGitit master
               => Page
               -> Maybe RevisionId
               -> GH master (Maybe ([GititToc], [Text], Html))
wikifyAndCache page mbrev = do
  pagePath  <- pathForPage page
  tocPath <- pathForToc page
  catPath <- pathForCategories page
  mbTocAndPageHtml <- do
    mbPageHtml <- tryPageCache pagePath
    mbTocPandoc <- tryJSONCache tocPath
    mbCatTexts <- tryJSONCache catPath
    return $ (,,) <$>  mbTocPandoc <*> mbCatTexts <*> mbPageHtml
  maybe (do
            mbcont <- getRawContents pagePath mbrev
            case mbcont of
              Just contents -> do
                          wikipage <- contentsToWikiPage page contents
                          htmlContents <- caching pagePath $ pageToHtml wikipage
                          let tocHierarchy = wpTocHierarchy wikipage
                              categories = wpCategories wikipage
                          cacheJSON tocPath tocHierarchy
                          cacheJSON catPath categories
                          return $ Just (tocHierarchy, categories, htmlContents)
              Nothing -> return Nothing)
      (return . Just)
      mbTocAndPageHtml

view :: HasGitit master => Maybe RevisionId -> Page -> GH master Html
view mbrev page = do
  mbTocAndPageHtml <- wikifyAndCache page mbrev
  case mbTocAndPageHtml of
    Just (tocHierarchy, categories, htmlContents) ->
             layout [ViewTab,EditTab,HistoryTab,DiscussTab]
                            tocHierarchy
                            categories
                            htmlContents
    Nothing -> do
         path' <- pathForFile page
         mbcont' <- getRawContents path' mbrev
         is_source <- isSourceFile path'
         case mbcont' of
              Nothing -> do
                 setMessageI (MsgNewPage page)
                 redirect $ EditR page
              Just contents
               | is_source -> do
                   htmlContents <- sourceToHtml path' contents
                   caching path' $ layout [ViewTab,HistoryTab] [] [] htmlContents
               | otherwise -> do
                  ct <- getMimeType path'
                  let content = toContent contents
                  caching path' (return (ct, content)) >>= sendResponse
   where layout tabs tocHierarchy categories cont = do
           toMaster <- getRouteToParent
           contw <- toWikiPage cont
           mbTocDepth <- toc_depth <$> getConfig
           mbToc <- extractToc mbTocDepth (pageToText page) tocHierarchy
           subpageTocInContent <- subpage_toc_in_content <$> getConfig
           makePage pageLayout{ pgName = Just page
                              , pgPageTools = True
                              , pgTabs = tabs
                              , pgSelectedTab = if isDiscussPage page
                                                   then DiscussTab
                                                   else ViewTab } $
                    do setTitle $ toMarkup page
                       toWidget [julius|
                                   $(document).keypress(function(event) {
                                       if (event.which == 18) {
                                          $.post("@{toMaster $ ExpireR page}");
                                          window.location.reload(true);
                                          };
                                       });
                                |]
                       when subpageTocInContent
                           (void $ toWidget [julius|
                                      $(".toc-subpage-link").each(function(_index, tocSubpageLink){
                                        var jTocSubpageLink = $(tocSubpageLink);
                                        var subpageLinkIdent = jTocSubpageLink.attr("id").substr(4);
                                        $("#" + subpageLinkIdent).html(jTocSubpageLink.clone().children());
                                      });
                                   |])
                       atomLink (toMaster $ AtomPageR page)
                          "Atom link for this page"
                       $(whamletFile "template/custom/view.hamlet")

extractTocAbs :: HasGitit master
              => Maybe Int
              -> (WriterOptions
                      -> Text
                      -> [a]
                      -> StateT PWH.WriterState (GH master) (Maybe Html))
              -> Text
              -> [a]
              -> GH master (Maybe (WidgetT master IO ()))
extractTocAbs mbTocDepth tocFun prefix tocHierrarchy = do
  let opts = def { writerWrapText = False
                 , writerHtml5 = True
                 , writerHighlight = True
                 , writerHTMLMathMethod = MathML Nothing
                 }
  let tocFunS = tocFun (maybe opts
                        (\tocDepth -> opts { writerTOCDepth = tocDepth})
                        mbTocDepth)
                prefix tocHierrarchy
  mbTocRendered <- evalStateT tocFunS PWH.defaultWriterState
  case mbTocRendered of
    Just tocRendered -> return $ Just $ toWidget tocRendered
    -- page has no titles and hence no toc
    Nothing -> return Nothing

extractToc :: HasGitit master
           => Maybe Int
           -> Text
           -> [GititToc]
           -> GH master (Maybe (WidgetT master IO ()))
extractToc mbTocDepth prefix = extractTocAbs mbTocDepth tableOfContents prefix

getIndexBaseR :: HasGitit master => GH master Html
getIndexBaseR = getIndexFor []

getIndexR :: HasGitit master => Page -> GH master Html
getIndexR (Page xs) = getIndexFor xs

getIndexFor :: HasGitit master => [Text] -> GH master Html
getIndexFor paths = do
  fs <- filestore <$> getYesod
  listing <- liftIO $ directory fs $ T.unpack $ T.intercalate "/" paths
  let isDiscussionPage (FSFile f) = isDiscussPageFile f
      isDiscussionPage (FSDirectory _) = return False
  prunedListing <- filterM (fmap not . isDiscussionPage) listing
  let updirs = inits $ filter (not . T.null) paths
  toMaster <- getRouteToParent
  let process (FSFile f) = do
        Page page <- pageForPath f
        ispage <- isPageFile f
        let route = toMaster $ ViewR $ Page (paths ++ page)
        return (if ispage then ("page" :: Text) else "upload", route, toMessage $ Page page)
      process (FSDirectory f) = do
        Page page <- pageForPath f
        let route = toMaster $ IndexR $ Page (paths ++ page)
        return ("folder", route, toMessage $ Page page)
  entries <- mapM process prunedListing
  makePage pageLayout{ pgName = Nothing } $ [whamlet|
    <h1 .title>
      $forall up <- updirs
        ^{upDir toMaster up}
    <div .index>
      <ul>
        $forall (cls,route,name) <- entries
          <li .#{cls}>
            <a href=@{route}>#{name}</a>
  |]

upDir :: (Route Gitit -> Route master) -> [Text] -> WidgetT master IO ()
upDir toMaster fs = do
  let (route, lastdir) = case reverse fs of
                          (f:_)  -> (IndexR $ Page fs, f)
                          []     -> (IndexBaseR, "\x2302")
  [whamlet|<a href=@{toMaster $ route}>#{lastdir}/</a>|]

getRawContents :: HasGitit master
               => FilePath
               -> Maybe RevisionId
               -> GH master (Maybe ByteString)
getRawContents path rev = do
  fs <- filestore <$> getYesod
  liftIO $ handle (\e -> if e == FS.NotFound then return Nothing else throw e)
         $ Just <$> retrieve fs path rev

pageToHtml :: HasGitit master => WikiPage -> GH master Html
pageToHtml wikiPage =
  return $ writeHtml def{
               writerWrapText = False
             , writerHtml5 = True
             , writerHighlight = True
             , writerHTMLMathMethod = MathML Nothing
             } $ Pandoc nullMeta (wpContent wikiPage)

stripHeader :: [ByteString] -> (ByteString,ByteString)
stripHeader (x:xs)
  | isHeaderStart x = let (hs, bs) = break isHeaderEnd xs
                      in  case bs of
                             []     -> (B.unlines (x:xs), B.empty)
                             (_:ys) -> (B.unlines hs, B.unlines ys)
  | otherwise = (B.empty, B.unlines (x:xs))
 where isHeaderStart z = ["---"] == B.words z
       isHeaderEnd   z = ["..."] == B.words z
stripHeader [] = (B.empty, B.empty)

contentsToWikiPage :: HasGitit master => Page  -> ByteString -> GH master WikiPage
contentsToWikiPage page contents = do
  conf <- getConfig
  plugins' <- getPlugins
  let (h,b) = stripHeader $ B.lines contents
  let metadata :: M.Map Text Value
      metadata = if B.null h
                    then M.empty
                    else fromMaybe M.empty
                         $ decode $! BS.concat $ B.toChunks h
  let formatStr = case M.lookup "format" metadata of
                         Just (String s) -> s
                         _               -> ""
  let format = fromMaybe (default_format conf) $ readPageFormat formatStr
  let readerOpts literate = def{ readerSmart = True
                               , readerExtensions =
                                   if literate
                                      then Set.insert Ext_literate_haskell pandocExtensions
                                      else pandocExtensions }
  let (reader, lhs) = case format of
                        Markdown l -> (readMarkdown (readerOpts l), l)
                        Textile  l -> (readTextile (readerOpts l), l)
                        LaTeX    l -> (readLaTeX (readerOpts l), l)
                        RST      l -> (readRST (readerOpts l), l)
                        HTML     l -> (readHtml (readerOpts l), l)
                        Org      l -> (readOrg (readerOpts l), l)
  let fromBool (Bool t) = t
      fromBool _        = False
  let toc = maybe False fromBool (M.lookup "toc" metadata)
  let doc = reader $ toString b
  let pageToPrefix (Page []) = T.empty
      pageToPrefix (Page ps) = T.intercalate "/" $ init ps ++ [T.empty]
  docWithIdForSubpage <- addSubpageClass doc
  Pandoc _ blocks <- sanitizePandoc <$> addWikiLinks (pageToPrefix page) docWithIdForSubpage
  let tocHierarchy = stripElementsForToc (extended_toc conf) 0 $ hierarchicalize blocks
  foldM applyPlugin
           WikiPage {
             wpName        = pageToText page
           , wpFormat      = format
           , wpTOC         = toc
           , wpLHS         = lhs
           , wpTitle       = toList $ text $ T.unpack $ pageToText page
           , wpCategories  = extractCategories metadata
           , wpMetadata    = metadata
           , wpCacheable   = True
           , wpContent     = blocks
           , wpTocHierarchy = tocHierarchy
           } plugins'

sourceToHtml :: HasGitit master
             => FilePath -> ByteString -> GH master Html
sourceToHtml path contents = do
  let formatOpts = defaultFormatOpts { numberLines = True, lineAnchors = True }
  return $ formatHtmlBlock formatOpts $
     case languagesByExtension $ takeExtension path of
        []    -> highlightAs "" $ toString contents
        (l:_) -> highlightAs l $ toString contents

mathjax_url :: Text
mathjax_url = "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

toWikiPage :: HasGitit master => Html -> GH master (WidgetT master IO ())
toWikiPage rendered = do
  cfg <- config <$> getYesod
  return $ do
    when (use_mathjax cfg) $ addScriptRemote mathjax_url
    toWidget rendered

postSearchR :: HasGitit master => GH master Html
postSearchR = do
  patterns <- lift $ runInputPost $ ireq textField "patterns"
  searchResults $ T.words patterns

postGoR :: HasGitit master => GH master Html
postGoR = do
  gotopage <- lift $ runInputPost $ ireq textField "gotopage"
  let gotopage' = T.toLower gotopage
  allPages <- allPageFiles
  let allPageNames = map (T.pack . dropExtension) allPages
  let findPage f   = find f allPageNames
  let exactMatch f = gotopage == f
  let insensitiveMatch f = gotopage' == T.toLower f
  let prefixMatch f = gotopage' `T.isPrefixOf` T.toLower f
  case findPage exactMatch `mplus` findPage insensitiveMatch `mplus`
        findPage prefixMatch of
       Just m  -> redirect $ ViewR $ textToPage m
       Nothing -> searchResults $ T.words gotopage

searchResults :: HasGitit master => [Text] -> GH master Html
searchResults patterns = do
  fs <- filestore <$> getYesod
  matchLines <- if null patterns
                   then return []
                   else liftIO $ search fs SearchQuery{
                                             queryPatterns =
                                                map T.unpack patterns
                                           , queryWholeWords = True
                                           , queryMatchAll = True
                                           , queryIgnoreCase = True
                                           }
  let contentMatches = map matchResourceName matchLines
  allPages <- allPageFiles
  let slashToSpace = map (\c -> if c == '/' then ' ' else c)
  let inPageName pageName' x = x `elem` words
           (slashToSpace $ dropExtension pageName')
  let matchesPatterns pageName' = not (null patterns) &&
       all (inPageName (map toLower pageName'))
           (map (T.unpack . T.toLower) patterns)
  let pageNameMatches = filter matchesPatterns allPages
  let allMatchedFiles = [f | f <- allPages, f `elem` contentMatches
                                     || f `elem` pageNameMatches ]
  let matchesInFile f =  mapMaybe (\x -> if matchResourceName x == f
                                            then Just (matchLine x)
                                            else Nothing) matchLines
  let matches = map (\f -> (f, matchesInFile f)) allMatchedFiles
  let relevance (f, ms) = length ms + if f `elem` pageNameMatches
                                         then 100
                                         else 0
  let matches' = sortBy (flip (comparing relevance)) matches
  let matches'' = map (\(f,c) -> (textToPage $ T.pack $ dropExtension f, c)) matches'
  toMaster <- getRouteToParent
  makePage pageLayout{ pgName = Nothing
                     , pgTabs = []
                     , pgSelectedTab = EditTab } $ do
    toWidget [julius|
      function toggleMatches(obj) {
        var pattern = $('#pattern').text();
        var matches = obj.next('.matches')
        matches.slideToggle(300);
        if (obj.html() == '\u25BC') {
            obj.html('\u25B2');
          } else {
            obj.html('\u25BC');
          };
        }
      $(document).ready(function (){
         $('a.showmatch').attr('onClick', 'toggleMatches($(this));');
         $('pre.matches').hide();
         $('a.showmatch').show();
         });
    |]
    [whamlet|
      <h1>#{T.unwords patterns}
      <ol>
        $forall (page, cont) <- matches''
          <li>
            <a href=@{toMaster $ ViewR page}>#{page}
            $if not (null cont)
               <a href="#" .showmatch>&#x25BC;
            <pre .matches>#{unlines cont}
    |]

getEditR :: HasGitit master => Page -> GH master Html
getEditR page = do
  requireUser
  fs <- filestore <$> getYesod
  path <- pathForPage page
  mbcont <- getRawContents path Nothing
  let contents = case mbcont of
                       Nothing -> ""
                       Just c  -> toString c
  mbrev <- case mbcont of
                 Nothing -> return Nothing
                 Just _  -> pathForPage page >>= \f ->
                              liftIO (Just <$> latest fs f)
  edit False contents mbrev page

getRevertR :: HasGitit master
           => RevisionId -> Page -> GH master Html
getRevertR rev page = do
  requireUser
  path <- pathForPage page
  mbcont <- getRawContents path (Just rev)
  case mbcont of
       Nothing       -> notFound
       Just contents -> edit True (toString contents) (Just rev) page

edit :: HasGitit master
     => Bool               -- revert?
     -> String             -- contents to put in text box
     -> Maybe RevisionId   -- unless new page, Just id of old version
     -> Page
     -> GH master Html
edit revert txt mbrevid page = do
  requireUser
  let contents = Textarea $ T.pack txt
  mr <- getMessageRender
  let comment = if revert
                   then mr $ MsgReverted $ fromMaybe "" mbrevid
                   else ""
  (form, enctype) <- lift $ generateFormPost $ editForm
                     $ Just Edit{ editContents = contents
                                , editComment = comment }
  toMaster <- getRouteToParent
  let route = toMaster $ case mbrevid of
                    Just revid -> UpdateR revid page
                    Nothing    -> CreateR page
  showEditForm page route enctype $ do
    when revert $ toWidget [julius|
       $(document).ready(function (){
          $('textarea').attr('readonly','readonly').attr('style','color: gray;');
          }); |]
    form

showEditForm :: HasGitit master
             => Page
             -> Route master
             -> Enctype
             -> WidgetT master IO ()
             -> GH master Html
showEditForm page route enctype form =
  makePage pageLayout{ pgName = Just page
                     , pgTabs = [EditTab]
                     , pgSelectedTab = EditTab }
  $ do
    toWidget [julius|
     function updatePreviewPane() {
       var url = location.pathname.replace(/_edit\//,"_preview/");
       $.post(
           url,
           {"contents" : $("#editpane").attr("value")},
           function(data) {
             $('#previewpane').html(data);
             // Process any mathematics if we're using MathML
             if (typeof(convert) == 'function') { convert(); }
             // Process any mathematics if we're using jsMath
             if (typeof(jsMath) == 'object')    { jsMath.ProcessBeforeShowing(); }
             // Process any mathematics if we're using MathJax
             if (typeof(window.MathJax) == 'object') {
               // http://docs.mathjax.org/en/latest/typeset.html
               var math = document.getElementById("MathExample");
               MathJax.Hub.Queue(["Typeset",MathJax.Hub,math]);
             }
           },
           "html");
     };   |]
    [whamlet|
      <h1>#{page}</h1>
      <div #editform>
        <form method=post action=@{route} enctype=#{enctype}>
          ^{form}
          <input type=submit>
          <input type=button onclick="updatePreviewPane()" value="Preview">
     <div #previewpane>
    |]

postUpdateR :: HasGitit master
          => RevisionId -> Page -> GH master Html
postUpdateR revid = update' (Just revid)

postCreateR :: HasGitit master
            => Page -> GH master Html
postCreateR = update' Nothing

update' :: HasGitit master
       => Maybe RevisionId -> Page -> GH master Html
update' mbrevid page = do
  user <- requireUser
  ((result, widget), enctype) <- lift $ runFormPost $ editForm Nothing
  fs <- filestore <$> getYesod
  toMaster <- getRouteToParent
  let route = toMaster $ case mbrevid of
                  Just revid  -> UpdateR revid page
                  Nothing     -> CreateR page
  case result of
       FormSuccess r -> do
         let auth = Author (gititUserName user) (gititUserEmail user)
         let comm = T.unpack $ editComment r
         let cont = filter (/='\r') $ T.unpack $ unTextarea $ editContents r
         path <- pathForPage page
         case mbrevid of
           Just revid -> do
              mres <- liftIO $ FS.modify fs path revid auth comm cont
              case mres of
                   Right () -> do
                      expireCache path
                      redirect $ ViewR page
                   Left mergeinfo -> do
                      setMessageI $ MsgMerged revid
                      edit False (mergeText mergeinfo)
                           (Just $ revId $ mergeRevision mergeinfo) page
           Nothing -> do
             expireCache path
             liftIO $ save fs path auth comm cont
             redirect $ ViewR page
       _ -> showEditForm page route enctype widget

data Edit = Edit { editContents :: Textarea
                 , editComment  :: Text
                 } deriving Show

editForm :: HasGitit master
         => Maybe Edit
         -> Html
         -> MForm (HandlerT master IO) (FormResult Edit, WidgetT master IO ())
editForm mbedit = renderDivs $ Edit
    <$> areq textareaField (fieldSettingsLabel MsgPageSource)
                             {fsId = Just "editpane"}
           (editContents <$> mbedit)
    <*> areq commentField (fieldSettingsLabel MsgChangeDescription)
           (editComment <$> mbedit)
  where commentField = check validateNonempty textField
        validateNonempty y
          | T.null y = Left MsgValueRequired
          | otherwise = Right y


getDiffR :: HasGitit master
         => RevisionId -> RevisionId -> Page -> GH master Html
getDiffR fromRev toRev page = do
  fs <- filestore <$> getYesod
  pagePath <- pathForPage page
  filePath <- pathForFile page
  rawDiff <- liftIO
             $ catch (diff fs pagePath (Just fromRev) (Just toRev))
             $ \e -> case e of
                      FS.NotFound -> diff fs filePath (Just fromRev) (Just toRev)
                      _           -> throw e
  makePage pageLayout{ pgName = Just page
                     , pgTabs = []
                     , pgSelectedTab = EditTab } $
   [whamlet|
     <h1 .title>#{page}
     <h2 .revision>#{fromRev} &rarr; #{toRev}
     <pre>
        $forall t <- rawDiff
           $case t
             $of Both xs _
               <span .unchanged>#{intercalate "\n" xs}
             $of First xs
               <span .deleted>#{intercalate "\n" xs}
             $of Second xs
               <span .added>#{intercalate "\n" xs}
     |]

getHistoryR :: HasGitit master
            => Int -> Page -> GH master Html
getHistoryR start page = do
  let items = 20 -- items per page
  fs <- filestore <$> getYesod
  pagePath <- pathForPage page
  filePath <- pathForFile page
  path <- liftIO
          $ catch (latest fs pagePath >> return pagePath)
          $ \e -> case e of
                   FS.NotFound -> latest fs filePath >> return filePath
                   _           -> throw e
  let offset = start - 1
  hist <- liftIO $ drop offset <$>
           history fs [path] (TimeRange Nothing Nothing) (Just $ start + items)
  histw <- mapM (revisionDetails False) hist
  let hist' = zip3 [(1 :: Int)..] hist histw
  toMaster <- getRouteToParent
  let pageForwardLink = if length hist > items
                           then Just $ toMaster
                                     $ HistoryR (start + items) page
                           else Nothing
  let pageBackLink    = if start > 1
                           then Just $ toMaster
                                     $ HistoryR (start - items) page
                           else Nothing
  let tabs = if path == pagePath
                then [ViewTab,EditTab,HistoryTab,DiscussTab]
                else [ViewTab,HistoryTab]
  makePage pageLayout{ pgName = Just page
                     , pgTabs = tabs
                     , pgSelectedTab = HistoryTab } $ do
   addScript $ staticR $ StaticRoute ["js","jquery-ui-1.8.21.custom.min.js"] []
   toWidget [julius|
      $(document).ready(function(){
          $(".difflink").draggable({helper: "clone"});
          $(".difflink").droppable({
               accept: ".difflink",
               drop: function(ev, ui) {
                  var diffurl = $(this).attr("diffurl");
                  var targetOrder = parseInt($(this).attr("order"));
                  var sourceOrder = parseInt($(ui.draggable).attr("order"));
                  if (targetOrder < sourceOrder) {
                      var toRev   = $(this).attr("revision");
                      var fromRev = $(ui.draggable).attr("revision");
                  } else {
                      var fromRev = $(this).attr("revision");
                      var toRev   = $(ui.draggable).attr("revision");
                  };
                  location.href = diffurl.replace('FROM',fromRev).replace('TO',toRev);
                  }
              });
      });
   |]
   [whamlet|
     <h1 .title>#{page}
     <p>_{MsgDragDiff}
     <ul>
       $forall (pos,rev,details) <- hist'
         <li .difflink order=#{pos} revision=#{revId rev} diffurl=@{toMaster $ DiffR "FROM" "TO" page}>
           ^{details}
     ^{pagination pageBackLink pageForwardLink}
     |]

revisionDetails :: HasGitit master
                => Bool     -- True = link to page history
                -> Revision
                -> GH master (WidgetT master IO ())
revisionDetails linkToPageHistory rev = do
  let toChange :: Change -> GH master (Text, Page)
      toChange (Modified f) = ("modified",) <$> pageForPath f
      toChange (Deleted  f) = ("deleted",)  <$> pageForPath f
      toChange (Added    f) = ("added",)    <$> pageForPath f
  toMaster <- getRouteToParent
  changes <- mapM toChange $ revChanges rev
  return $ [whamlet|
    <span .date>#{show $ revDateTime rev}
    (<span .author>#{authorName $ revAuthor rev}</span>):
    <span .subject>#{revDescription rev}
    $forall (cls,pg) <- changes
      $if linkToPageHistory
        <a href=@{toMaster $ HistoryR 1 pg}>
          <span .#{cls}>#{pg}
      $else
        <a href=@{toMaster $ RevisionR (revId rev) pg}>
          <span .#{cls}>#{pg}
  |]

pagination :: HasGitit master
           => Maybe (Route master)    -- back link
           -> Maybe (Route master)    -- forward link
           -> WidgetT master IO ()
pagination pageBackLink pageForwardLink =
   [whamlet|
     <p .pagination>
       $maybe bl <- pageBackLink
         <a href=@{bl}>&larr;
       &nbsp;
       $maybe fl <- pageForwardLink
         <a href=@{fl}>&rarr;
     |]

getActivityR :: HasGitit master
              => Int -> GH master Html
getActivityR start = do
  let items = 20
  let offset = start - 1
  fs <- filestore <$> getYesod
  hist <- liftIO $ drop offset <$>
           history fs [] (TimeRange Nothing Nothing) (Just $ start + items)
  hist' <- mapM (revisionDetails True) hist
  toMaster <- getRouteToParent
  let pageForwardLink = if length hist > items
                           then Just $ toMaster
                                     $ ActivityR (start + items)
                           else Nothing
  let pageBackLink    = if start > 1
                           then Just $ toMaster
                                     $ ActivityR (start - items)
                           else Nothing
  makePage pageLayout{ pgName = Nothing
                     , pgTabs = []
                     , pgSelectedTab = HistoryTab }
   [whamlet|
     <h1 .title>Recent activity
     <ul>
       $forall details <- hist'
         <li>
           ^{details}
     ^{pagination pageBackLink pageForwardLink}
    |]

getAtomSiteR :: HasGitit master => GH master RepAtom
getAtomSiteR = do
  tryCache "_feed"
  caching "_feed" $ feed Nothing >>= atomFeed

getAtomPageR :: HasGitit master => Page -> GH master RepAtom
getAtomPageR page = do
  path <- pathForPage page
  tryCache (path </> "_feed")
  caching (path </> "_feed") $ feed (Just page) >>= atomFeed

feed :: HasGitit master
     => Maybe Page  -- page, or nothing for all
     -> GH master (Feed (Route Gitit))
feed mbpage = do
  days <- feed_days <$> getConfig
  mr <- getMessageRender
  fs <- filestore <$> getYesod
  now <- liftIO getCurrentTime
  paths <- case mbpage of
                Just p  -> (:[]) <$> pathForPage p
                Nothing -> return []
  let startTime = addUTCTime (fromIntegral $ -60 * 60 * 24 * days) now
  revs <- liftIO $ history fs paths
           TimeRange{timeFrom = Just startTime,timeTo = Nothing}
           (Just 200) -- hard limit of 200 to conserve resources
  let toEntry rev = do
        let topage change = case change of
                              Modified f -> ("" :: Text,) <$> pageForPath f
                              Deleted f  -> ("-",) <$> pageForPath f
                              Added f    -> ("+",) <$> pageForPath f
        firstpage <- case revChanges rev of
                           []    -> error "feed - encountered empty changes"
                           (c:_) -> snd <$> topage c
        let toChangeDesc c = do
             (m, pg) <- topage c
             return $ m <> pageToText pg
        changeDescrips <- mapM toChangeDesc $ revChanges rev
        return FeedEntry{
                   feedEntryLink    = RevisionR (revId rev) firstpage
                 , feedEntryUpdated = revDateTime rev
                 , feedEntryTitle   = T.intercalate ", " changeDescrips <> ": "
                                      <> T.pack (revDescription rev) <> " (" <>
                                      T.pack (authorName $ revAuthor rev) <> ")"
                 , feedEntryContent = toHtml $ T.pack ""
                 }
  entries <- mapM toEntry [rev | rev <- revs, not (null $ revChanges rev) ]
  return Feed{
        feedAuthor = ""
      , feedTitle = mr $ maybe MsgSiteFeedTitle MsgPageFeedTitle mbpage
      , feedLinkSelf = maybe AtomSiteR AtomPageR mbpage
      , feedLinkHome = HomeR
      , feedDescription = undefined -- only used for rss
      , feedLanguage = undefined    -- only used for rss
      , feedUpdated = now
      , feedEntries = entries
    }

getExportR :: HasGitit master
            => Text
            -> Page
            -> GH master (ContentType, Content)
getExportR format page = do
  exportFormats <- getExportFormats
  case lookup format exportFormats of
         Nothing -> fail "Unrecognized format"
         Just (extension, f) -> do
           path <- pathForPage page
           -- set filename here so it works for cached page
           setFilename $ pageToText page <> extension
           tryCache $ path </> T.unpack format
           mbcont <- getRawContents path Nothing
           case mbcont of
                Nothing   -> fail "Could not get page contents"
                Just cont -> contentsToWikiPage page cont >>=
                               caching (path </> T.unpack format) . f >>=
                               sendResponse

-- TODO:
-- fix mime types
-- handle math in html formats
-- other slide show issues (e.g. dzslides core)
-- add pdf, docx, odt, epu

pureWriter :: (WriterOptions -> Pandoc -> String) -> WriterOptions -> Pandoc -> IO String
pureWriter w opts d = return $ w opts d

toSelfContained :: FilePath -> WriterOptions -> String -> IO String
toSelfContained repopath w cont =
  inDirectory repopath $ makeSelfContained w cont

getExportFormats :: GH master [(Text, (Text, WikiPage -> GH master (ContentType,Content)))]
getExportFormats = do
  conf <- getConfig
  let repopath = repository_path conf
  let userdata = pandoc_user_data conf
  let selfcontained = toSelfContained repopath
  return $
    [ ("Asciidoc", (".txt", basicExport "asciidoc" typePlain $ pureWriter writeAsciiDoc))
    , ("Beamer", (".tex", basicExport "beamer" "application/x-latex" $ pureWriter writeLaTeX))
    , ("ConTeXt", (".tex", basicExport "context" "application/x-context" $ pureWriter writeConTeXt))
    , ("DocBook", (".xml", basicExport "docbook" "application/docbook+xml" $ pureWriter writeDocbook))
    , ("DZSlides", (".html", basicExport "dzslides" typeHtml $ \opts -> selfcontained opts .
                writeHtmlString opts{ writerSlideVariant = DZSlides
                              , writerHtml5 = True }))
    , ("EPUB", (".epub", basicExport "epub" "application/xhtml+xml" $ \opts ->
                 inDirectory repopath . writeEPUB opts))
    , ("Groff man", (".1", basicExport "man" typePlain $ pureWriter writeMan))
    , ("HTML", (".html", basicExport "html" typeHtml $ \opts -> selfcontained opts . writeHtmlString opts))
    , ("HTML5", (".html", basicExport "html5" typeHtml $ \opts ->
                   selfcontained opts . writeHtmlString opts{ writerHtml5 = True }))
    , ("LaTeX", (".tex", basicExport "latex" "application/x-latex" $ pureWriter writeLaTeX))
    , ("Markdown", (".txt", basicExport "markdown" typePlain $ pureWriter writeMarkdown))
    , ("Mediawiki", (".wiki", basicExport "mediawiki" typePlain $ pureWriter writeMediaWiki))
    , ("ODT", (".odt", basicExport "opendocument" "application/vnd.oasis.opendocument.text"
             $ \opts -> inDirectory repopath . writeODT opts))
    , ("OpenDocument", (".xml", basicExport "opendocument" "application/vnd.oasis.opendocument.text"
                   $ pureWriter writeOpenDocument))
    , ("Org-mode", (".org", basicExport "org" typePlain $ pureWriter writeOrg)) ] ++
    [ ("PDF", (".pdf", basicExport "latex" "application/pdf" $ \opts d ->
                   inDirectory repopath $ makePDF (fromMaybe "pdflatex" $
                     latex_engine conf) writeLaTeX opts d >>= \res ->
                       case res of
                         Left e    -> error $ "Could not produce PDF: " ++ toString e
                         Right pdf -> return pdf)) | isJust (latex_engine conf) ] ++
    [ ("Plain text", (".txt", basicExport "plain" typePlain $ pureWriter writePlain))
    , ("reStructuredText", (".txt", basicExport "rst" typePlain $ pureWriter writeRST))
    , ("RTF", (".rtf", basicExport "rtf" "application/rtf" writeRTFWithEmbeddedImages))
    , ("Textile", (".txt", basicExport "textile" typePlain $ pureWriter writeTextile))
    , ("S5", (".html", basicExport "s5" typeHtml $ \opts ->
                selfcontained opts . writeHtmlString opts{ writerSlideVariant = S5Slides }))
    , ("Slidy", (".html", basicExport "slidy" typeHtml $ \opts ->
                selfcontained opts . writeHtmlString opts{ writerSlideVariant = SlidySlides }))
    , ("Texinfo", (".texi", basicExport "texinfo" "application/x-texinfo" $ pureWriter writeTexinfo))
    , ("Word docx", (".docx", basicExport "docx"
            "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            $ \opts -> inDirectory repopath . writeDocx opts))
    ]

basicExport :: ToContent a
            => String -> ContentType -> (WriterOptions -> Pandoc -> IO a)
            -> WikiPage -> GH master (ContentType, Content)
basicExport templ contentType writer wikiPage = do
  conf <- getConfig
  template' <- liftIO $ getDefaultTemplate (pandoc_user_data conf) templ
  template <- case template' of
                     Right t  -> return t
                     Left e   -> throw e
  let metadataToVar :: (Text, Value) -> Maybe (String, String)
      metadataToVar (k, String v) = Just (T.unpack k, T.unpack v)
      metadataToVar (k, Bool v)   = Just (T.unpack k, if v then "yes" else "no")
      metadataToVar (k, Number v) = Just (T.unpack k, show v)
      metadataToVar _             = Nothing
  let vars = mapMaybe metadataToVar $ M.toList $ wpMetadata wikiPage
  dzcore <- if templ == "dzslides"
                then liftIO $ do
                  dztempl <- readDataFileUTF8 (pandoc_user_data conf)
                             $ "dzslides" </> "template.html"
                  return $ unlines
                      $ dropWhile (not . isPrefixOf "<!-- {{{{ dzslides core")
                      $ lines dztempl
                else return ""
  rendered <- liftIO
              $ writer def{
                         writerTemplate = template
                       , writerSourceURL = Just $ repository_path conf
                       , writerStandalone = True
                       , writerExtensions = if wpLHS wikiPage
                                               then Set.insert Ext_literate_haskell pandocExtensions
                                               else pandocExtensions
                       , writerTableOfContents = wpTOC wikiPage
                       , writerHTMLMathMethod = MathML Nothing
                       , writerVariables = ("dzslides-core",dzcore):vars }
              $ Pandoc (Meta $ M.singleton "title" $ MetaInlines $ wpTitle wikiPage) $ wpContent wikiPage
  return (contentType, toContent rendered)

setFilename :: Text -> HandlerT sub (HandlerT master IO) ()
setFilename fname = addHeader "Content-Disposition"
                  $ "attachment; filename=\"" <> fname <> "\""

getUploadR :: HasGitit master => GH master Html
getUploadR = do
  requireUser
  (form, enctype) <- lift $ generateFormPost $ uploadForm Nothing
  showUploadForm enctype form

showUploadForm :: HasGitit master
               => Enctype
               -> WidgetT master IO ()
               -> GH master Html
showUploadForm enctype form = do
  toMaster <- getRouteToParent
  makePage pageLayout{ pgName = Nothing
                     , pgTabs = []
                     , pgSelectedTab = EditTab } $ do
    toWidget $ [julius|
      $(document).ready(function(){
          $("#file").change(function () {
            var fn = $(this).val().replace(/.*\\/,"");
            $("#wikiname").val(fn);
          });
        });
    |]
    [whamlet|
      <h1>_{MsgUploadFile}</h1>
      <div #uploadform>
        <form method=post action=@{toMaster UploadR} enctype=#{enctype}>
          ^{form}
          <input type=submit>
    |]

data Upload = Upload { uploadFile        :: FileInfo
                     , uploadWikiname    :: Text
                     , uploadDescription :: Text
                     , uploadOverwrite   :: Bool
                     }

uploadForm :: HasGitit master
           => Maybe Upload
           -> Html
           -> MForm (HandlerT master IO) (FormResult Upload, WidgetT master IO ())
uploadForm mbupload =
  renderDivs $ Upload
     <$> fileAFormReq (fieldSettingsLabel MsgFileToUpload){
                fsId = Just "file" }
     <*> areq pageField (fieldSettingsLabel MsgWikiName){
                fsId = Just "wikiname" } (uploadWikiname <$> mbupload)
     <*> areq commentField (fieldSettingsLabel MsgChangeDescription)
            (uploadDescription <$> mbupload)
     <*> areq checkBoxField (fieldSettingsLabel MsgOverwrite)
            (uploadOverwrite <$> mbupload)
   where commentField = check validateNonempty textField
         validateNonempty y
           | T.null y = Left MsgValueRequired
           | otherwise = Right y
         pageField = check validatePage textField
         validatePage x = case fromPathMultiPiece (T.splitOn "/" x) of
                                Just (_ :: Page) -> Right x
                                Nothing          -> Left MsgInvalidPageName


postUploadR :: HasGitit master => GH master Html
postUploadR = do
  user <- requireUser
  ((result, widget), enctype) <- lift $ runFormPost $ uploadForm Nothing
  fs <- filestore <$> getYesod
  case result of
       FormSuccess r -> do
         let fileinfo = uploadFile r
         let page = textToPage $ uploadWikiname r
         let auth = Author (gititUserName user) (gititUserEmail user)
         let comm = T.unpack $ uploadDescription r
         path <- pathForFile page
         allfiles <- liftIO $ index fs
         if path `elem` allfiles && not (uploadOverwrite r)
            then do
              setMessageI MsgFileExists
              showUploadForm enctype widget
            else do
              cont <- lift $ fromChunks <$> (fileSource fileinfo $$ consume)
              res <- liftIO $ try $ save fs path auth comm cont
              case res of
                   Left FS.Unchanged -> do
                                        setMessageI MsgFileUnchanged
                                        showUploadForm enctype widget
                   Left e            -> throw e
                   Right _           -> do
                                        expireCache path
                                        redirect $ ViewR page
       _             -> showUploadForm enctype widget

----------
-- Caching
--
-- We cache Blah.page as Blah.page/_page.html, and any of its exports
-- as Blah.page/EXPORT_FORMAT/filename.  Remove the whole Blah.page directory
-- expires all of them.  Non-pages Foo.jpg just get cached as Foo.jpg.
----------

postExpireHomeR :: HasGitit master => GH master Html
postExpireHomeR = do
  conf <- getConfig
  postExpireR $ textToPage $ front_page conf

postExpireR :: HasGitit master => Page -> GH master Html
postExpireR page = do
  useCache <- use_cache <$> getConfig
  when useCache $
     do
       pathForPage page >>= expireCache
       pathForFile page >>= expireCache
  redirect $ ViewR page

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

cacheJSON :: ToJSON a => FilePath -> a -> GH master ()
cacheJSON cachepath a = do
  conf <- getConfig
  when (use_cache conf) $ liftIO $ do
                          let fullpath = cache_dir conf </> cachepath
                          createDirectoryIfMissing True $ takeDirectory fullpath
                          B.writeFile fullpath (ASON.encode a)

tryPageCache :: FilePath -> GH master (Maybe Html)
tryPageCache  = processDirCache
                (\ fullpath x -> do
                   pageString <- liftIO $ TIO.readFile $ fullpath </> x
                   return $ Just $ preEscapedToHtml pageString)

tryJSONCache :: FromJSON a => FilePath -> GH master (Maybe [a])
tryJSONCache = processDirCache
                 (\ fullpath x -> liftIO $
                     withFile (fullpath </> x) ReadMode $ \hnd -> do
                       pageString <- hGetContents hnd
                       return $ ASON.decode pageString)

tryCatCache :: FilePath -> GH master (Maybe [Text])
tryCatCache = processDirCache
                 (\ fullpath x -> liftIO $
                     withFile (fullpath </> x) ReadMode $ \hnd -> do
                       pageString <- BS.hGetContents hnd
                       -- TODO: simplify
                       return $ Just $ map T.pack $ read $ show pageString)
tryCache :: FilePath -> GH master ()
tryCache = void .
           processDirCache
           (\ fullpath x -> do
              let ct = BSU.fromString $ urlDecode x
              sendFile ct $ fullpath </> x
              return Nothing)

processDirCache :: (FilePath ->FilePath -> GH master (Maybe a))
                -> FilePath
                -> GH master (Maybe a)
processDirCache process path = do
  conf <- getConfig
  if use_cache conf
     then do
       let fullpath = cache_dir conf </> path
       exists <- liftIO $ doesDirectoryExist fullpath
       if exists
          then (do
            files <- liftIO $ getDirectoryContents fullpath >>=
                               filterM (doesFileExist . (fullpath </>))
            case files of
                 (x:_) -> process fullpath x
                 _     -> return Nothing)
           else return Nothing
     else return Nothing

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

-- categories ------------

-- NOTE:  The current implementation of of categories does not go via the
-- filestore abstraction.  That is bad, but can only be fixed if we add
-- more sophisticated searching options to filestore.

getCategoriesR :: HasGitit master => GH master Html
getCategoriesR = do
  tryCache "_categories"
  conf <- getConfig
  toMaster <- getRouteToParent
  let repopath = repository_path conf
  allpages <- map (repopath </>) <$> allPageFiles
  allcategories <- liftIO $ nub . sort . concat <$> mapM readCategories allpages
  caching "_categories" $
    makePage pageLayout{ pgName = Nothing
                       , pgTabs = []
                       , pgSelectedTab = EditTab }
    [whamlet|
      <h1>_{MsgCategories}</h1>
      <ul>
        $forall category <- allcategories
          <li><a href=@{toMaster $ CategoryR category}>#{category}
    |]

getCategoryR :: HasGitit master => Text -> GH master Html
getCategoryR category = do
  let cachepage = "_categories" </> T.unpack category
  tryCache cachepage
  conf <- getConfig
  toMaster <- getRouteToParent
  let repopath = repository_path conf
  allpages <- allPageFiles
  let hasCategory pg = elem category <$> readCategories (repopath </> pg)
  matchingpages <- mapM pageForPath =<< (sort <$> filterM (liftIO . hasCategory) allpages)
  caching cachepage $
    makePage pageLayout{ pgName = Nothing
                       , pgTabs = []
                       , pgSelectedTab = EditTab }
    [whamlet|
      <h1>_{MsgCategory}: #{category}</h1>
      <ul.index>
        $forall page <- matchingpages
          <li .page><a href=@{toMaster $ ViewR page}>#{page}
    |]

-- | Examine metadata at beginning of file, returning list of categories.
-- Note:  Must be strict.
readCategories :: FilePath -> IO [Text]
readCategories f = do
  hdr <- getHeader f
  return $ if BS.null hdr
     then []
     else extractCategories $ fromMaybe M.empty $ decode hdr

extractCategories :: M.Map Text Value -> [Text]
extractCategories metadata =
  case M.lookup ("categories" :: Text) metadata of
       Just (String t) -> T.words $ T.replace "," " " t
       _               -> []

getHeader :: FilePath -> IO BS.ByteString
getHeader f =
  withFile f ReadMode $ \h ->
    catch (do fl <- BS.hGetLine h
              if dashline fl
                 then BSC.unlines <$> hGetLinesTill h dotline
                 else return BS.empty)
       (\e -> if isEOFError e then return BS.empty else throw e)

dashline :: BS.ByteString -> Bool
dashline x =
  case BSC.unpack x of
       ('-':'-':'-':xs) | all (==' ') xs -> True
       _ -> False

dotline :: BS.ByteString -> Bool
dotline x =
  case BSC.unpack x of
       ('.':'.':'.':xs) | all (==' ') xs -> True
       _ -> False

hGetLinesTill :: Handle -> (BS.ByteString -> Bool) -> IO [BS.ByteString]
hGetLinesTill h end = do
  next <- BS.hGetLine h
  if end next
     then return []
     else do
       rest <- hGetLinesTill h end
       return (next:rest)

-- | Extends an HTML writer from a single GititToc to [GititToc]
tableOfContentsAbs :: (WriterOptions
                           -> Text
                           -> a
                           -> StateT PWH.WriterState (GH master) (Maybe Html))
                   -> WriterOptions
                   -> Text
                   -> [a]
                   -> StateT PWH.WriterState (GH master) (Maybe Html)
tableOfContentsAbs _ _ _ [] = return Nothing
tableOfContentsAbs writer opts prefix sects = do
  contents  <- mapM (writer opts { writerIgnoreNotes = True } prefix) sects
  return $ case catMaybes contents of
             [] -> Nothing
             tocList -> Just $ PWH.unordList opts tocList

-- | HTML writer from [GititToc] to HTML
tableOfContents :: HasGitit master
                => WriterOptions
                -> Text
                -> [GititToc]
                -> StateT PWH.WriterState (GH master) (Maybe Html)
tableOfContents = tableOfContentsAbs gititTocToListItem

-- | Convert section number to string
showSecNum :: [Int] -> String
showSecNum = intercalate "." . map show

-- | HTML writer from a single GititToc to HTML
gititTocToListItem :: HasGitit master
                   => WriterOptions
                   -> Text
                   -> GititToc
                   -> StateT PWH.WriterState (GH master) (Maybe Html)
-- Don't include the empty headers created in slide shows
-- shows when an hrule is used to separate slides without a new title:
gititTocToListItem _ _ (GititSec _ _ _ [Str "\0"] _) = return Nothing

gititTocToListItem opts prefix (GititSec lev num (id',classes,_) headerText subsecs)
  | lev <= writerTOCDepth opts = do
  let num' = zipWith (+) num (writerNumberOffset opts ++ repeat 0)
  let sectnum = if writerNumberSections opts && not (null num) &&
                   "unnumbered" `notElem` classes
                   then (H.span ! A.class_ "toc-section-number"
                        $ toHtml $ showSecNum num') >> preEscapedToHtml (" " :: String)
                   else mempty
  txt <- liftM (sectnum >>) $ PWH.inlineListToHtml opts headerText
  subHeads <- liftM catMaybes (mapM (gititTocToListItem opts prefix) subsecs)
  let subList = if null subHeads
                   then mempty
                   else PWH.unordList opts subHeads
  -- in reveal.js, we need #/apples, not #apples:
  let revealSlash = ['/' | writerSlideVariant opts == RevealJsSlides]
  if null id'
              then return $ Just $ H.a (toHtml txt) >> subList
              else do
                  toMaster <- lift getRouteToParent
                  let route = ViewR $ textToPage prefix
                  toUrl <- lift $ lift getUrlRender
                  return $ Just $ (H.a ! A.href
                                          (toValue $ T.unpack (toUrl $ toMaster route)
                                           ++ "#" ++ revealSlash ++ writerIdentifierPrefix opts ++ id')
                                        $ toHtml txt) >> subList

gititTocToListItem opts _ (GititLink lev attr ref (s, tit))
  | lev <= writerTOCDepth opts = do
  let target = textToPage $ T.pack $ inlinesToString ref
  linkText <- PWH.inlineListToHtml opts ref
  let s' = case s of
             '#':xs | writerSlideVariant opts == RevealJsSlides -> '#':'/':xs
             _ -> s
  let link = H.a ! A.href (toValue s') $ linkText
      link' = if ref == [Str (unEscapeString s)]
                 then link ! A.class_ "uri"
                 else link
      link'' = if null tit
                  then link'
                  else link' ! A.title (toValue tit)
  mbTocAndPageHtml <- lift $ wikifyAndCache target Nothing
  case mbTocAndPageHtml of
    Just (tocs, _, _) -> do
                          mbToc <- tableOfContents
                                   opts { writerTOCDepth = writerTOCDepth opts - lev }
                                   (pageToText target) tocs
                          case mbToc of
                            Just toc -> return $ Just $
                                        let (ident, classes, _) = attr in
                                        H.div ! A.id (toValue ("toc-" ++ ident))
                                             ! A.class_ (toValue ("toc-" ++  head classes)) $
                                        link'' >> toc
                            -- referred page has no toc
                            Nothing -> return $ Just link''
    -- referred page does not exist or could not be parsed
    Nothing -> return $ Just link''

gititTocToListItem _ _ _ = return Nothing

-- stolen from gitit ContentTransfomers.hs
-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
               Str s                   -> s
               Emph xs                 -> concatMap go xs
               Strong xs               -> concatMap go xs
               Strikeout xs            -> concatMap go xs
               Superscript xs          -> concatMap go xs
               Subscript xs            -> concatMap go xs
               SmallCaps xs            -> concatMap go xs
               Quoted DoubleQuote xs   -> '"' : (concatMap go xs ++ "\"")
               Quoted SingleQuote xs   -> '\'' : (concatMap go xs ++ "'")
               Cite _ xs               -> concatMap go xs
               Code _ s                -> s
               Space                   -> " "
               LineBreak               -> " "
               Math DisplayMath s      -> "$$" ++ s ++ "$$"
               Math InlineMath s       -> "$" ++ s ++ "$"
               RawInline (Format "tex") s -> s
               RawInline _ _           -> ""
               Link xs _               -> concatMap go xs
               Image xs _              -> concatMap go xs
               Note _                  -> ""
               Span _ xs               -> concatMap go xs

-- | Keeps only sections and links from Elements
stripElementsForToc :: Bool -> Int -> [Element] -> [GititToc]
stripElementsForToc extToc lev elements =
    concat  $ fmap (stripElementForToc extToc  lev) elements

stripElementForToc :: Bool -> Int -> Element -> [GititToc]
stripElementForToc extToc _ (Sec lev num attr headerText subsecs) =
    [GititSec lev num attr headerText (stripElementsForToc extToc (lev + 1) subsecs)]
stripElementForToc False _ _  = []
stripElementForToc True lev (Blk block) =
    fmap (\(attr, inlines, target) -> (GititLink lev attr inlines target)) (fetchLink block)


fetchLink :: Block -> [(Text.Pandoc.Attr, [Inline], Target)]
fetchLink = queryWith isLinkInPar
    where isLinkInPar (Div (ident,["subpage-link"],[]) [Para [Link inlines target]])
              = [((ident,["subpage-link"],[]), inlines, target)]
          isLinkInPar _ = []

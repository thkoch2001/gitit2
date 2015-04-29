{-# LANGUAGE TupleSections #-}

module Network.Gitit2.Handler.History (
  getActivityR,
  getHistoryR
  ) where

import           Control.Exception (catch, throw)
import           Data.FileStore (authorName, Change(Added, Deleted, Modified), history, latest,
                                 revAuthor, revChanges, revDateTime, revDescription, revId,
                                 Revision, TimeRange(TimeRange))
import qualified Data.FileStore as FS
import           Network.Gitit2.Import
import           Network.Gitit2.Page (pathForFile)
import           Yesod.Static (Route(StaticRoute))

getHistoryR :: HasGitit master
            => Int -> Page -> GH master Html
getHistoryR start page = do
  let items = 20 -- items per page
  fs <- filestore <$> getYesod
  pagePath <- pathForPage page
  let filePath = pathForFile page
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

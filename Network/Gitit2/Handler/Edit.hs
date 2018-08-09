module Network.Gitit2.Handler.Edit (
  getEditR,
  getRevertR,
  postUpdateR,
  postCreateR
  ) where

import           Control.Monad  (when)
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.FileStore as FS
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Network.Gitit2.Cache
import           Network.Gitit2.Import
import Yesod (Route)

getEditR :: HasGitit master => Page -> GH master Html
getEditR page = do
  requireEditor
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
  requireEditor
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
  requireEditor
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
             // TODO: Process any mathematics (we only use mathjax as of 2015/04/07)
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
  user <- requireEditor
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
              mres <- liftIO $ modify fs path revid auth comm cont
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

{-# LANGUAGE ScopedTypeVariables #-}

module Network.Gitit2.Handler.Upload (
  getUploadR,
  postUploadR
  ) where

import           Control.Exception (try)
import           Data.ByteString.Lazy (fromChunks)
import           Data.Conduit (($$))
import           Data.Conduit.List (consume)
import           Data.FileStore (Author(Author), index, save)
import qualified Data.FileStore as FS
import qualified Data.Text as T
import           Network.Gitit2.Cache (expireCache)
import           Network.Gitit2.Import
import           Network.Gitit2.Page (pathForFile, textToPage)
import Control.Exception (throw)

getUploadR :: HasGitit master => GH master Html
getUploadR = do
  requireEditor
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
  user <- requireEditor
  ((result, widget), enctype) <- lift $ runFormPost $ uploadForm Nothing
  fs <- filestore <$> getYesod
  case result of
       FormSuccess r -> do
         let fileinfo = uploadFile r
         let page = textToPage $ uploadWikiname r
         let auth = Author (gititUserName user) (gititUserEmail user)
         let comm = T.unpack $ uploadDescription r
         let path = pathForFile page
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

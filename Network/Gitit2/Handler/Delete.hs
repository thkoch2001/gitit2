module Network.Gitit2.Handler.Delete (
  getDeleteR,
  postDeleteR
                                     ) where

import qualified Data.Text as T
import Network.Gitit2.Import
import Control.Exception (try)
import Data.FileStore (latest)
import qualified Data.FileStore as FS
import Network.Gitit2.Page (pathForFile)

getDeleteR :: HasGitit master => Page -> GH master Html
getDeleteR page = do
  requireEditor
  fs <- filestore <$> getYesod
  path <- pathForPage page
  pageTest <- liftIO $ try $ latest fs path
  fileToDelete <- case pageTest of
                       Right _        -> return path
                       Left  FS.NotFound -> do
                         let path' = pathForFile page
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
  user <- requireEditor
  fs <- filestore <$> getYesod
  mr <- getMessageRender
  fileToDelete <- lift $ runInputPost $ ireq textField "fileToDelete"
  liftIO $ FS.delete fs (T.unpack fileToDelete)
            (FS.Author (gititUserName user) (gititUserEmail user))
            (T.unpack $ mr $ MsgDeleted page)
  setMessageI $ MsgDeleted page
  redirect HomeR

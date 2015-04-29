module Network.Gitit2.Handler.Index (
  getIndexBaseR,
  getIndexR
  ) where

import           Control.Monad (filterM)
import           Data.FileStore as FS
import           Data.List (inits)
import qualified Data.Text as T
import           Network.Gitit2.Import
import           Network.Gitit2.Page (Page(..))
import           Yesod (Route)


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

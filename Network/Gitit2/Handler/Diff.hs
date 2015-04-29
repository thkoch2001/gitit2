module Network.Gitit2.Handler.Diff (
  getDiffR
  ) where

import           Control.Exception (catch, throw)
import           Data.FileStore (diff, RevisionId)
import qualified Data.FileStore as FS
import           Data.List (intercalate)
import           Network.Gitit2.Import
import           Network.Gitit2.Page (pathForFile)

getDiffR :: HasGitit master
         => RevisionId -> RevisionId -> Page -> GH master Html
getDiffR fromRev toRev page = do
  fs <- filestore <$> getYesod
  pagePath <- pathForPage page
  let filePath = pathForFile page
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
             $of FS.Both xs _
               <span .unchanged>#{intercalate "\n" xs}
             $of FS.First xs
               <span .deleted>#{intercalate "\n" xs}
             $of FS.Second xs
               <span .added>#{intercalate "\n" xs}
     |]

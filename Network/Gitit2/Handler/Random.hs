module Network.Gitit2.Handler.Random (
  getRandomR
  ) where

import Control.Monad (filterM)
import Data.FileStore (index)
import Network.Gitit2.Import
import System.Random (randomRIO)

getRandomR :: HasGitit master => GH master Html
getRandomR = do
  fs <- filestore <$> getYesod
  files <- liftIO $ index fs
  pages <- mapM pageForPath =<< filterM (fmap not . isDiscussPageFile)
                            =<< filterM isPageFile files
  pagenum <- liftIO $ randomRIO (0, length pages - 1)
  let thepage = pages !! pagenum
  redirect $ ViewR thepage

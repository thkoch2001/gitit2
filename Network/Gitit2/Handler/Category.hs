module Network.Gitit2.Handler.Category (
  getCategoriesR,
  getCategoryR
  ) where

import           Control.Exception (catch, throw)
import           Control.Monad (filterM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.List (nub, sort)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Yaml (decode)
import           Network.Gitit2.Cache
import           Network.Gitit2.Import
import           Network.Gitit2.WikiPage (extractCategories)
import           System.FilePath ((</>))
import           System.IO (Handle, withFile, IOMode(ReadMode))
import           System.IO.Error (isEOFError)

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

module Network.Gitit2.Handler.Search (
  postGoR,
  postSearchR
  ) where

import Network.Gitit2.Import
import qualified Data.Text as T
import System.FilePath (dropExtension)
import           Data.List (find, sortBy)
import Control.Monad (mplus)
import           Network.Gitit2.Page (textToPage)
import Data.FileStore (search)
import Data.FileStore (SearchQuery(..))
import Data.FileStore (matchResourceName)
import           Data.Char (toLower)
import           Data.Maybe (mapMaybe)
import Data.FileStore (matchLine)
import Data.Ord (comparing)

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


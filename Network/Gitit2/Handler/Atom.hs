{-# LANGUAGE TupleSections #-}

module Network.Gitit2.Handler.Atom (
  getAtomPageR,
  getAtomSiteR
  ) where

import           Data.FileStore
import qualified Data.Text as T
import           Data.Time (addUTCTime, getCurrentTime)
import           Network.Gitit2.Cache
import           Network.Gitit2.Import
import           Network.Gitit2.Page (pageToText)
import           System.FilePath ((</>))
import           Yesod (Route)
import           Yesod.AtomFeed

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

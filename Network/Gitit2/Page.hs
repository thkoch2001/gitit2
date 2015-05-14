module Network.Gitit2.Page
       where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath (dropExtension, takeExtension)
import           Text.Blaze (toMarkup, ToMarkup)
import           Yesod (fromPathMultiPiece, PathMultiPiece, toMessage, ToMessage, toPathMultiPiece)

-- | Path to a wiki page.  Page and page components can't begin with '_'.
data Page = Page [Text] deriving (Show, Read, Eq)

-- for now, we disallow @*@ and @?@ in page names, because git filestore
-- does not deal with them properly, and darcs filestore disallows them.
instance PathMultiPiece Page where
  toPathMultiPiece (Page x) = x
  fromPathMultiPiece []     = Nothing
  fromPathMultiPiece xs@(_:_) =
     if any (\x ->  "_" `T.isPrefixOf` x ||
                    "*" `T.isInfixOf` x ||
                    "?" `T.isInfixOf` x ||
                    ".." `T.isInfixOf` x ||
                    "/_" `T.isInfixOf` x) xs
                    then Nothing
                    else Just (Page xs)

pageToText :: Page -> Text
pageToText (Page xs) = T.intercalate "/" xs

textToPage :: Text -> Page
textToPage x = Page $ T.splitOn "/" x

instance ToMarkup Page where
  toMarkup (Page ps) = (toMarkup . last) ps

instance ToMessage Page where
  toMessage = pageToText

type PageExtension = FilePath

pathForPageP :: PageExtension -> Page -> FilePath
pathForPageP pageExtension page = T.unpack (toMessage page) <> pageExtension

pathForFile :: Page -> FilePath
pathForFile = T.unpack . toMessage

pageForPathP :: PageExtension -> FilePath -> Page
pageForPathP pageExtension fp =
  textToPage $ T.pack $ if takeExtension fp == pageExtension
    then dropExtension fp
    else fp

isDiscussPage :: Page -> Bool
isDiscussPage (Page xs) =
  case reverse xs of
    (x:_) -> "@" `T.isPrefixOf` x
    _     -> False

discussPageFor :: Page -> Page
discussPageFor (Page xs)
  | isDiscussPage (Page xs) = Page xs
  | otherwise               = Page $ init xs ++ ["@" <> last xs]

discussedPage :: Page -> Page
discussedPage (Page xs)
  | isDiscussPage (Page xs) = Page $ init xs ++ [T.drop 1 $ last xs]
  | otherwise               = Page xs

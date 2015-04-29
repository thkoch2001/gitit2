{-# LANGUAGE CPP #-}

module Network.Gitit2.Import
    ( module Import,
      Page
    ) where

#if __GLASGOW_HASKELL__ <= 704
import           Prelude              as Import hiding (catch, head, init, last,
                                                 readFile, tail, writeFile)

#else
import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
#endif


import           Yesod                as Import hiding (Route (..), MsgDelete)

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)

import           Network.Gitit2.Foundation as Import
import           Network.Gitit2.Helper as Import
import           Network.Gitit2.Page (Page)
-- import           Settings             as Import
-- import           Settings.Development as Import
-- import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

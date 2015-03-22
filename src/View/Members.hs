{-# LANGUAGE OverloadedStrings #-}

module View.Members (
      memberView
    ) where

import Prelude hiding (div, head, id)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Time
import Data.Text (append)
import Text.Blaze (customAttribute)
import Text.Blaze.Html
import Text.Blaze.Html5 (a, div, h1, h2, h3, nav, li, link, p, ul, title,
                        table, td, tr, th, (!))
import Text.Blaze.Html5.Attributes (class_, href, id, media, name, rel, src,
                                   type_)
import Control.Monad (foldM)
import View.Bootstrap
import Web.Scotty

import Model.Members

memberView :: ActionM ()
memberView = do
    m <- liftIO getMembers
    blaze . bootstrap (cssStarter "thug") $ do
        navbar
        div ! class_ "container" $ div ! class_ "starter-template" $ do
            h2 "thugs"
            listMembers m
        pageFooter

listMembers :: [Member] -> Html
listMembers m = table ! class_ "table table-striped" $ do
    tr $ do
        th $ "name"
        th $ "email"
        th $ "joined"
    foldl fillTable mempty m
    where
        fillTable acc (Member f l e _ j) = acc `mappend` row
            where
                row = tr $ do
                    td . toHtml $ f `append` " " `append` l
                    td $ toHtml e
                    td . toHtml . show $ utctDay j


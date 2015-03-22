{-# LANGUAGE OverloadedStrings #-}

module View.Events (
      eventView
    ) where

import Prelude hiding (div, head, id)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
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

import Model.Thug

eventView :: ActionM ()
eventView = do
    e <- liftIO getEvents
    blaze . bootstrap (cssStarter "thug") $ do
        navbar
        div ! class_ "container" $ div ! class_ "starter-template" $ do
            h2 "thug events"
            listEvents e
        pageFooter

listEvents :: [Event] -> Html
listEvents e = table ! class_ "table table-striped" $ do
    tr $ do
        th "event"
        th "date"
        th "description"
    foldl fillTable mempty e
    where
        fillTable acc (Event n d t) = acc `mappend` row
            where
                row = tr $ do
                    td $ toHtml n
                    td . toHtml $ show t
                    td $ toHtml d


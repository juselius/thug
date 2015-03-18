{-# LANGUAGE OverloadedStrings #-}

module Views.Home (
      homeView
    , coverView
    ) where

import Prelude hiding (div, head, id)
import Text.Blaze (customAttribute)
import Text.Blaze.Html5 (a, div, h1, h2, h3, nav, li, link, p, ul, (!), title)
import Text.Blaze.Html5.Attributes (class_, href, id, media, name, rel, src,
                                   type_)
import Views.Bootstrap
import Web.Scotty (ActionM)

coverView :: ActionM()
coverView = blaze $ bootstrap coverHead $ do
    div ! class_ "site-wrapper" $
        div ! class_ "site-wrapper-inner" $
            div ! class_ "cover-container" $ do
                div ! class_ "masthead clearfix" $ do
                    div ! class_ "inner" $ do
                        h3 ! class_ "masthead-brand" $ "thug"
                        nav $ ul ! class_ "nav masthead-nav" $ do
                            li ! class_ "active" $ a ! href "/home" $ "home"
                            li $ a ! href "/contact" $ "contact"
                            li $ a ! href "/join" $ "join"
                div ! class_ "inner cover" $ do
                    h1 ! class_ "cover-heading" $ "tromsø haskell users group"
                    p ! class_ "lead" $ "Cover is a one-page temp"
                    p ! class_ "lead" $
                        a   ! href "/home"
                            ! class_ "btn btn-lg btn-default" $ "learn more"
                div ! class_ "mastfoot" $
                    div ! class_ "inner" $
                        p "thug 2014"

homeView :: ActionM ()
homeView = blaze $ bootstrap (title "thug home") $ do
    navbar
    div ! class_ "jumbotron" $
        div ! class_ "container" $ do
            h1 "tromsø haskell users group"
            p "welcome to tromsø haskell users group!"
    div ! class_ "container" $
        div ! class_ "row" $ do
            div ! class_ "col-md-4" $ do
                h2 "thug"
                p "not really."
                detailsButton "#"
            div ! class_ "col-md-4" $ do
                h2 "life"
                p "conway."
                detailsButton "#"
            div ! class_ "col-md-4" $ do
                h2 "hacker"
                p "heck yes!"
                detailsButton "#"
    pageFooter


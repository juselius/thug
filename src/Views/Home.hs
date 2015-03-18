{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView) where

import Prelude hiding (div, head, id)
import Text.Blaze (customAttribute)
import Text.Blaze.Html5 (a, div, h1, h2, li, link, p, ul, (!))
import Text.Blaze.Html5.Attributes (class_, href, id, media, name, rel, src,
                                   type_)
import Views.Bootstrap
import Web.Scotty (ActionM)

homeView :: ActionM ()
homeView = blaze $ bootstrap "home" $ do
    div ! class_ "jumbotron" $
        div ! class_ "container" $ do
            h1 "Tromsø Haskell Users Group"
            p "Welcome to Tromsø Haskell Users Group!"
    div ! class_ "container" $
        div ! class_ "row" $ do
            div ! class_ "col-md-4" $ do
                h2 "Thug"
                p "Not really."
                detailsButton "#"
            div ! class_ "col-md-4" $ do
                h2 "Life"
                p "Conway."
                detailsButton "#"
            div ! class_ "col-md-4" $ do
                h2 "Hacker"
                p "Heck yes!"
                detailsButton "#"





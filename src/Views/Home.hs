{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView) where

import Prelude hiding (div, head, id)
import Text.Blaze.Html5 (a, div, h1, li, link, p, ul, (!))
import Text.Blaze.Html5.Attributes ( class_, href, id, media, name, rel, src,
                                   type_)
import Views.Bootstrap (bootstrap, blaze)
import Web.Scotty (ActionM)

homeView :: ActionM ()
homeView = blaze $ bootstrap "home" $ do
    div ! class_ "container" $ do
        div ! class_ "jumbotron" $ do
            h1 "THUG! Tromsø Haskell Users Group"
            p "Welcome to Tromsø Haskell Users Group!"
            p $ do
                a   ! class_ "btn btn-lg btn-primary"
                    ! id "fb"
                    ! href "#navbar" $ "Facebook"
                a   ! class_ "btn btn-lg btn-danger"
                    ! id "gmail"
                    ! href "#navbar" $ "Gmail"


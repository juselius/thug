{-# LANGUAGE OverloadedStrings #-}

module Views.Join (
      joinView
    , joinHandler
    ) where

import Prelude hiding (div, head, id)
import Text.Blaze (customAttribute)
import Text.Blaze.Html
import Text.Blaze.Html5 (
    a, div, h1, h2, h3, nav, li, link, p, ul, (!), title, form, input, button)
import Text.Blaze.Html5.Attributes (
    class_, href, id, media, name, rel, src, type_, placeholder, action, method)
import Views.Bootstrap
import Web.Scotty
import Data.Aeson (object, (.=))
import qualified Data.Text as T

joinView :: ActionM()
joinView = blaze $ bootstrap headerBasic $ do
    navbar
    div ! class_ "container" $
        div ! class_ "starter-template" $ do
            h1 "tromsø haskell users group"
            p "please join tromsø haskell users group!"
            div ! class_ "container" $
                div ! class_ "row" $ do
                    h2 "thug"
                    p "not really."
                    form ! action "join" ! method "POST" $ do
                        div ! class_ "form-group" $
                            input   ! type_ "text"
                                    ! placeholder "Email"
                                    ! name "email"
                                    ! class_ "form-control"
                        div ! class_ "form-group" $
                            input   ! type_ "password"
                                    ! placeholder "Password"
                                    ! name "passwd"
                                    ! class_ "form-control"
                        button  ! type_ "submit"
                                ! class_ "btn btn-success" $ "Sign in"
    pageFooter

joinHandler :: ActionM()
joinHandler = do
    emailAddress <- param "email" :: ActionM T.Text
    passwd <- param "passwd" :: ActionM T.Text
    -- registered <- liftIO (registerInterest emailAddress)
    -- case registered of
    --     Just errorMessage -> do
    --         json $ object [ "error" .= errorMessage ]
    --         status internalServerError500
    --     Nothing -> do
    -- json $ object [ "ok" .= ("ok" :: String) ]
    blaze $ bootstrap headerBasic $ do
        navbar
        div ! class_ "container" $
            div ! class_ "starter-template" $ do
                p $ toHtml emailAddress
                p $ toHtml passwd

headerBasic :: Html
headerBasic = do
    title "join thug"
    link ! href "/custom/stater-template.css" ! rel "stylesheet"

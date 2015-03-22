{-# LANGUAGE OverloadedStrings #-}

module View.Join (
      joinView
    , joinHandler
    ) where

import Prelude hiding (div, head, id)
import Data.Monoid (mempty)
import Text.Blaze (customAttribute)
import Text.Blaze.Html
import Text.Blaze.Html5 (a, div, h1, h2, h3, nav, li, link, p, ul,
                        (!), title, form, input, button, table, tr, td)
import Text.Blaze.Html5.Attributes (class_, href, id, media, name, rel, src,
                                   required, type_, placeholder, action,
                                   method, onsubmit, onclick)
import Web.Scotty
import Network.HTTP.Types.Status
import Data.Aeson (object, (.=))
import qualified Data.Text as T

import View.Bootstrap
import Controller.Join

joinView :: ActionM()
joinView = blaze . bootstrap (cssStarter "join thug") $ do
    navbar
    div ! class_ "container" $
        div ! class_ "starter-template" $ do
            h1 "tromsø haskell users group"
            p . toHtml $ unwords [
                  "please join tromsø haskell users group!"
                , "functional programmers can join too!"
                ]
            joinForm
    pageFooter

joinForm :: Html
joinForm = form
    ! action "join"
    ! method "POST"
    ! onsubmit "return Strict.Thug.validateJoin(true).value"
    ! id "joinForm" $
    table ! class_ "table table-hover" $ do
        tr $ do
            td "first name"
            td $ div ! class_ "form-group" $
                input ! type_ "text"
                      ! placeholder "First name"
                      ! name "firstname"
                      ! required "true"
                      ! id "joinFirstName"
                      ! class_ "form-control"
        tr $ do
            td "last name"
            td $ div ! class_ "form-group" $
                input ! type_ "text"
                      ! placeholder "Last name"
                      ! name "lastname"
                      ! required "true"
                      ! id "joinLastName"
                      ! class_ "form-control"
        tr $ do
            td "email"
            td $ div ! class_ "form-group" $
                input ! type_ "email"
                      ! placeholder "Email"
                      ! name "email"
                      ! required "true"
                      ! id "joinEmail"
                      ! class_ "form-control"
        tr $ do
            td "password"
            td $ table ! class_ "table-condensed" $ do
                tr . td $ div ! class_ "form-group" $
                    input ! type_ "password"
                          ! placeholder "Password"
                          ! name "passwd"
                          ! required "true"
                          ! id "joinPasswd"
                          ! class_ "form-control"
                tr . td $ div ! class_ "form-group" $
                    input ! type_ "password"
                          ! placeholder "Verify Password"
                          ! name "passwd_"
                          ! required "true"
                          ! id "joinPasswd_"
                          ! class_ "form-control"
                tr. td $ div ! id "errorBox" $ mempty
        tr $ do
            td mempty
            td $ button
                ! type_ "submit"
                ! class_ "btn btn-success" $ "Register"


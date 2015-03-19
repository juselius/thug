{-# LANGUAGE OverloadedStrings #-}

module Views.Join (
      joinView
    , joinHandler
    ) where

import Prelude hiding (div, head, id)
import Data.Monoid (mempty)
import Text.Blaze (customAttribute)
import Text.Blaze.Html
import Text.Blaze.Html5 (
    a, div, h1, h2, h3, nav, li, link, p, ul, (!), title, form, input,
    button, table, tr, td)
import Text.Blaze.Html5.Attributes (
    class_, href, id, media, name, rel, src, type_, placeholder, action, method)
import Views.Bootstrap
import Web.Scotty
import Network.HTTP.Types.Status
import Data.Aeson (object, (.=))
import qualified Data.Text as T

joinView :: ActionM()
joinView = blaze $ bootstrap headerBasic $ do
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
joinForm = form ! action "join" ! method "POST" $
    table ! class_ "table table-hover" $ do
        tr $ do
            td "full name"
            td $ div ! class_ "form-group" $
                input ! type_ "text"
                      ! placeholder "Name"
                      ! name "fullname"
                      ! class_ "form-control"
        tr $ do
            td "email"
            td $ div ! class_ "form-group" $
                input ! type_ "text"
                      ! placeholder "Email"
                      ! name "email"
                      ! class_ "form-control"
        tr $ do
            td "password"
            td $ table ! class_ "table-condensed" $ do
                tr . td $ div ! class_ "form-group" $
                    input ! type_ "password"
                          ! placeholder "Password"
                          ! name "passwd1"
                          ! class_ "form-control"
                tr . td $ div ! class_ "form-group" $
                        input ! type_ "password"
                              ! placeholder "Verify Password"
                              ! name "passwd2"
                              ! class_ "form-control"
        tr $ do
            td mempty
            td $ button ! type_ "submit"
                   ! class_ "btn btn-success" $ "Register"

joinHandler :: ActionM()
joinHandler = do
    fullname <- param "fullname" :: ActionM T.Text
    email    <- param "email"    :: ActionM T.Text
    passwd1  <- param "passwd1"  :: ActionM T.Text
    passwd2  <- param "passwd2"  :: ActionM T.Text
    -- registered <- liftIO (registerInterest emailAddress)
    -- case registered of
    --     Just errorMessage -> do
    --         json $ object [ "error" .= errorMessage ]
    --         status internalServerError500
    --     Nothing -> do
    -- json $ object [ "ok" .= ("ok" :: String) ]
    if passwd1 /= passwd2
        then do
            page $ p "Passwords don't match!"
            status badRequest400
        else page $ do
            p $ toHtml fullname
            p $ toHtml email
            p $ toHtml . take (T.length passwd1) $ cycle ("*" :: String)
        where
            page x =
                blaze $ bootstrap headerBasic $ do
                    navbar
                    div ! class_ "container" $
                        div ! class_ "starter-template" $ x

headerBasic :: Html
headerBasic = do
    title "join thug"
    link ! href "/custom/starter-template.css" ! rel "stylesheet"

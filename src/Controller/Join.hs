{-# LANGUAGE OverloadedStrings #-}

module Controller.Join (
      joinHandler
    , joinHandlerJson
    ) where

import Prelude hiding (div, head, id)
import Data.Monoid (mempty)
import Text.Blaze (customAttribute)
import Text.Blaze.Html
import Text.Blaze.Html5 (a, div, h1, h2, h3, nav, li, link, p, ul, title,
                        form, input, button, table, tr, td, (!))
import Text.Blaze.Html5.Attributes (class_, href, id, media, name, rel, src,
                                   type_, placeholder, action, method)
import Web.Scotty
import Network.HTTP.Types.Status
import Data.Aeson (object, (.=))
import qualified Data.Text as T

import View.Bootstrap

joinHandler :: ActionM()
joinHandler = do
    fullname <- param "fullname" :: ActionM String
    email    <- param "email"    :: ActionM String
    passwd  <- param "passwd"  :: ActionM String
    passwd_  <- param "passwd_"  :: ActionM String
    if passwd /= passwd_
        then do
            page $ p "Passwords don't match!"
            status badRequest400
        else page $ do
            h2 . toHtml $ "Welcome thug " ++ fullname
            p $ toHtml fullname
            p $ toHtml email
            p $ toHtml . take (length passwd) $ cycle ("*" :: String)
        where
            page x =
                blaze . bootstrap (cssStarter "register") $ do
                    navbar
                    div ! class_ "container" $
                        div ! class_ "starter-template" $ x

joinHandlerJson :: ActionM()
joinHandlerJson = do
    -- join <- liftIO (registerInterest email)
    -- case registered of
        -- Just errorMessage -> do
            -- json $ object [ "error" .= errorMessage ]
            -- status internalServerError500
        -- Nothing -> do
    json $ object [ "ok" .= ("ok" :: String) ]


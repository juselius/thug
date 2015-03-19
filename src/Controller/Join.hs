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
    fullname <- param "fullname" :: ActionM T.Text
    email    <- param "email"    :: ActionM T.Text
    passwd1  <- param "passwd1"  :: ActionM T.Text
    passwd2  <- param "passwd2"  :: ActionM T.Text
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


{-# LANGUAGE OverloadedStrings #-}

module Controller.Join (
      joinHandler
    , addThug
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
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets

import View.Bootstrap
import Model.Thug

joinHandler :: ActionM()
joinHandler = do
    firstname <- param "firstname" :: ActionM String
    lastname  <- param "lastname" :: ActionM String
    email     <- param "email"     :: ActionM String
    passwd    <- param "passwd"    :: ActionM String
    passwd_   <- param "passwd_"   :: ActionM String
    let fullname = firstname ++ " " ++ lastname
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

addThug :: Connection -> BL.ByteString -> IO ()
addThug conn msg = do
    let Just usr = decode msg :: Maybe Member
    ok <- hasMember usr
    if ok
        then sendTextData conn ("user exists" :: T.Text)
        else do
            addMember usr
            sendTextData conn ("ok" :: T.Text)


{-# LANGUAGE OverloadedStrings #-}

module View.Bootstrap (
      bootstrap
    , navbar
    , pageFooter
    , blaze
    , cssStarter
    , cssCover
    , cssJumbo
    , detailsButton
    ) where

import Prelude hiding (div, head, id, span)
import Data.Monoid (mempty)
import Text.Blaze (dataAttribute, customAttribute, AttributeValue)
import Text.Blaze.Html (toHtml, Html, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 (a, body, button,  div, docTypeHtml, h1, head, li,
                        input, nav, hr, link, meta, p, script, style, title,
                        ul, (!), span, form, footer)
import Text.Blaze.Html5.Attributes (charset, class_, content, href, httpEquiv,
                                   id, media, name, rel, src, type_,
                                   placeholder)
import Web.Scotty (ActionM, html)
import qualified Data.Text.Lazy as T

bootstrap :: Html -> Html -> Html
bootstrap prelude contents = docTypeHtml $ do
    head $ do
        meta ! charset "utf-8"
        meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        meta ! name "description" ! content "Thug Life Haskeller"
        meta ! name "author" ! content "A.U. Thor "
        link ! href "/favicon.ico" ! rel "icon"
        link ! href "/css/bootstrap.min.css" ! rel "stylesheet"
        ieHacks
        prelude
    body $ do
        contents
        script ! src jqry $ mempty
        script ! src "/js/bootstrap.min.js" $ mempty
        script ! src "/js/thug.js" $ mempty

navbar :: Html
navbar = nav ! class_ "navbar navbar-inverse navbar-fixed-top" $
    div ! class_ "container" $ do
        div ! class_ "navbar-header" $ do
            button  ! type_ "button"
                    ! class_ "navbar-toggle"
                    ! dataAttribute "toggle" "collapse"
                    ! dataAttribute "target" "#navbar"
                    ! customAttribute "aria-expanded" "false"
                    ! customAttribute "aria-controls" "navbar" $ do
                        span ! class_ "sr-only" $ "Toggle navigation"
                        span ! class_ "icon-bar" $ mempty
                        span ! class_ "icon-bar" $ mempty
                        span ! class_ "icon-bar" $ mempty
            a ! class_ "navbar-brand" ! href "/home#about"   $ "about"
            a ! class_ "navbar-brand" ! href "/home#contact" $ "contact"
            a ! class_ "navbar-brand" ! href "/members" $ "members"
            a ! class_ "navbar-brand" ! href "/events"  $ "events"
        -- loginbar

loginbar :: Html
loginbar = div ! id "navbar"
    ! class_ "navbar-collapse collapse" $
    div ! class_ "container" $
        form ! class_ "navbar-form navbar-right" $ do
            div ! class_ "form-group" $
                input   ! type_ "text"
                        ! placeholder "Email"
                        ! class_ "form-control"
            div ! class_ "form-group" $
                input   ! type_ "password"
                        ! placeholder "Password"
                        ! class_ "form-control"
            button  ! type_ "submit"
                    ! class_ "btn btn-success" $ "Sign in"

detailsButton :: AttributeValue -> Html
detailsButton dest = p $ a
    ! class_ "btn btn-default"
    ! href dest
    ! customAttribute "role" "button" $
        preEscapedToHtml ("View details &raquo;":: T.Text)

pageFooter :: Html
pageFooter = div ! class_ "container" $ do
    hr
    footer . p $  preEscapedToHtml (
        "&copy; thug 2015. powered by haskell." :: T.Text
        )

blaze :: Html -> ActionM ()
blaze = html . T.pack . renderHtml

cssStarter :: String -> Html
cssStarter t = do
    title $ toHtml t
    link ! href "/custom/starter-template.css" ! rel "stylesheet"

cssCover :: String -> Html
cssCover t = do
    title $ toHtml t
    link ! href "/custom/cover.css" ! rel "stylesheet"

cssJumbo :: String -> Html
cssJumbo t = do
    title $ toHtml t
    link ! href "/custom/jumbotron.css" ! rel "stylesheet"


ieHacks :: Html
ieHacks = preEscapedToHtml $ unlines [
      "<!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->"
    , "<!--[if lt IE 9]>"
    , "<script src=\"https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js\"></script>"
    , "<script src=\"https://oss.maxcdn.com/respond/1.4.2/respond.min.js\"></script>"
    , "<![endif]-->"
    ]

jqry :: AttributeValue
jqry = "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"

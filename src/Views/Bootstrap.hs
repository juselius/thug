{-# LANGUAGE OverloadedStrings #-}

module Views.Bootstrap (bootstrap, blaze) where

import Prelude hiding (div, head, id)
import Data.Monoid (mempty)
import Data.Text.Lazy (toStrict)
import Text.Blaze.Html (Html, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (a, body, button, dataAttribute, div ,
                        docTypeHtml, h1, head, li, link, meta, p, script,
                        style, title, ul, (!))
import Text.Blaze.Html5.Attributes (charset, class_, content, href, httpEquiv,
                                   id, media, name, rel, src, type_)
import Web.Scotty (ActionM, html)

bootstrap :: Html -> Html -> Html
bootstrap t b = docTypeHtml $ do
    head $ do
        title t
        meta ! charset "utf-8"
        meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
        meta ! name "description" ! content "Thug Life Haskeller"
        meta ! name "viewport" ! content "width=device-width"
        link ! href "/favicon.ico" ! rel "icon"
        link ! href "/css/bootstrap.min.css" ! rel "stylesheet"
        link ! href "/custom/jumbotron.css"  ! rel "stylesheet"
        ieHacks
    body $ do
        navBar >> b
        script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
        script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty
        script ! src "/js/thug.js" $ mempty

navBar :: Html
navBar =
    div ! class_ "navbar navbar-inverse navbar-fixed-top" $
        div ! class_ "container" $ do
            div ! class_ "navbar-header" $ do
                button  ! type_ "button"
                        ! class_ "navbar-toggle"
                        ! dataAttribute "toggle" "collapse"
                        ! dataAttribute "target" ".navbar-collapse" $ do
                            a ! class_ "navbar-brand" ! href "#" $ "λ"
            div ! class_ "navbar-collapse collapse" $ ul
                ! class_ "nav navbar-nav" $ do
                    li ! class_ "active" $ a ! href "#" $ "Home"
                    li $ a ! href "#about" $ "About"
                    li $ a ! href "#contact" $ "Contact"
                    li $ a ! href "/members" $ "Members"
                    li $ a ! href "/events" $ "Events"

blaze :: Html -> ActionM ()
blaze = html . renderHtml

ieHacks :: Html
ieHacks = preEscapedToHtml $ unlines [
      "<!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->"
    , "<!--[if lt IE 9]>"
    , "<script src=\"https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js\"></script>"
    , "<script src=\"https://oss.maxcdn.com/respond/1.4.2/respond.min.js\"></script>"
    , "<![endif]-->"
    ]

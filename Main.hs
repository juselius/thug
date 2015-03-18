-- | Simple website for Tromsø Haskell Users Group, THUG.
--
-- Written by jonas.jusliusuit.no, 2015
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Network.Wai.Middleware.Static
import Views.Home
import Views.Join

main = scotty 3000 $ do
    middleware . staticPolicy $ addBase "static"
    get "/" coverView
    get "/home" homeView
    get "/join" joinView
    post "/join" joinHandler
    get "/html" $ file "html/index.html"
    get "/cover" $ file "html/cover.html"



-- | Simple website for Tromsø Haskell Users Group, THUG.
--
-- Written by jonas.jusliusuit.no, 2015
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Network.Wai.Middleware.Static
import Views.Home

main = scotty 3000 $ do
    middleware . staticPolicy $ addBase "static"
    get "/" homeView
    get "/html" $ file "html/index.html"



-- | Simple website for Tromsø Haskell Users Group, THUG.
--
-- Written by jonas.jusliusuit.no, 2015
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Network.Wai.Middleware.Static

main = scotty 3000 $ do
    middleware . staticPolicy $ addBase "static"
    get "/" $ file "html/index.html"



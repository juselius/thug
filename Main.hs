-- | Simple website for Tromsø Haskell Users Group, THUG.
--
-- Written by jonas.jusliusuit.no, 2015
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.WebSockets
import Control.Concurrent
import Control.Monad
import Data.Text

import View.Home
import View.Join
import Model.Members

main = do
    migrateDb
    void . forkIO $ runServer "127.0.0.1" 8080 wsHandler
    scotty 3000 $ do
        middleware . staticPolicy $ addBase "static"
        get "/" coverView
        get "/home" homeView
        get "/join" joinView
        post "/join" joinHandler
        get "/html" $ file "html/index.html"
        get "/cover" $ file "html/cover.html"

wsHandler :: ServerApp
wsHandler client = do
    conn <- acceptRequest client
    msg <- receiveData conn :: IO Text
    print msg


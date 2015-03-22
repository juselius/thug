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
import System.Posix.Files (fileExist)

import View.Home
import View.Join
import View.Members
import View.Events
import Model.Thug

main = do
    oldDb <- fileExist dbName
    migrateDb
    if oldDb
        then return ()
        else do
            addStaticMembers
            addStaticEvents
    Just m <- getMemberByEmail "jonas.juselius@uit.no"
    print m
    void . forkIO $ runServer "127.0.0.1" 8080 wsHandler
    scotty 3000 $ do
        middleware . staticPolicy $ addBase "static"
        get "/" coverView
        get "/home" homeView
        get "/join" joinView
        post "/join" joinHandler
        get "/members" memberView
        get "/events" eventView
        get "/html" $ file "html/index.html"
        get "/cover" $ file "html/cover.html"

wsHandler :: ServerApp
wsHandler client = do
    conn <- acceptRequest client
    msg <- receiveData conn :: IO Text
    print msg


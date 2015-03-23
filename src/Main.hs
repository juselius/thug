-- | Simple website for Tromsø Haskell Users Group, THUG.
--
-- Written by jonas.jusliusuit.no, 2015
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.WebSockets
import Data.Text
import Data.Time
import System.Posix.Files (fileExist)
import Control.Concurrent
import Control.Monad

import View.Home
import View.Join
import View.Members
import View.Events
import Model.Thug
import Controller.Join

main :: IO ()
main = do
    oldDb <- fileExist dbName
    migrateDb
    if oldDb
        then return ()
        else do
            addStaticMembers
            addStaticEvents
    void . forkIO $ runServer "127.0.0.1" 8080 wsHandler
    scotty 3000 $ do
        middleware . staticPolicy $ addBase "static"
        get "/" coverView
        get "/home" homeView
        get "/join" joinView
        post "/join" joinHandler
        get "/members" memberView
        get "/events" eventView
        get "/cover" $ file "html/cover.html"

wsHandler :: ServerApp
wsHandler client = do
    conn <- acceptRequest client
    op <- receiveDataMessage conn
    Text msg <- receiveDataMessage conn
    case op of
        Text "new thug" -> addThug conn msg
        _ -> sendTextData conn ("invalid operation" :: Text)


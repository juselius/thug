{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Model.Events (
      getEvents
    , addEvent
    , addStaticEvents
    ) where
import Prelude as P
import Control.Monad (void)
import Data.Text
import Data.Time
import Database.Persist hiding ((==.))
import Database.Persist.Sqlite (runSqlite)

import Model.Tables

type EventName = Text
type EventDesc = Text
type EventTime = String

addStaticEvents :: IO ()
addStaticEvents = runSqlite db $ do
    void $ insertMany
        [ Event "Meetup!" "Haskell hackatron 1"
            (read "2015-03-23 11:30:00 CET" :: UTCTime)
        , Event "Hackatron!" "Haskell hackatron 2"
            (read "2015-03-24 11:30:00 CET" :: UTCTime)]

addEvent :: EventName -> EventDesc -> EventTime -> IO ()
addEvent n d t = runSqlite db $
    void . insert $ Event n d (read t :: UTCTime)

getEvents :: IO [Event]
getEvents = runSqlite db $ do
    events <- selectList [] []
    return $ P.map (\(Entity _ x) -> x) events


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Model.Members (
      addStaticMembers
    , addMember
    , getMemberByEmail
    , getMembers
    , Member(..)
    ) where
import Prelude as P
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Text
import Data.Time
import Database.Persist hiding ((==.))
import Database.Persist.Sqlite (runSqlite)

import Model.Tables

type FirstName = Text
type LastName  = Text
type EmailAddr = Text
type Passwd    = Text

addStaticMembers :: IO ()
addStaticMembers = runSqlite db $ do
    now <- liftIO getCurrentTime
    void $ insert (Member "Marve" "Fleksnes" "marve@fleksnes.no" "secret" now)
    void $ insertMany [
          Member "Jonas" "Juselius" "jonas.juselius@uit.no" "secret" now
        ]

addMember :: FirstName -> LastName -> EmailAddr -> Passwd -> IO ()
addMember f l e p = runSqlite db $ do
    now <- liftIO getCurrentTime
    void $ insert (Member f l e p now)

getMembers :: IO [Member]
getMembers = runSqlite db $ do
    members <- selectList [] []
    return $ P.map (\(Entity _ x) -> x) members

getMemberByEmail :: Text -> IO (Maybe Member)
getMemberByEmail e = runSqlite db $ do
    member <- getBy $ UniqueEmail e
    case member of
        Just (Entity _ x) -> return $ Just x
        Nothing           -> return Nothing


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Model.Members (
      migrateDb
    , dbName
    , addStaticData
    , addMember
    , getMemberByEmail
    ) where
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Text
import Data.Time
import Database.Esqueleto
import Database.Persist hiding ((==.))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH

type FirstName = Text
type LastName  = Text
type Email     = Text
type Passwd    = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Member
  firstName Text
  lastName Text
  email Text
  password Text
  since UTCTime
  Email email     -- uniqueness constraint
  deriving Show

|]

dbName :: FilePath
dbName = "thug.db"

db :: Text
db = pack dbName

migrateDb :: IO ()
migrateDb = runSqlite db $ runMigration migrateAll

addStaticData :: IO ()
addStaticData = runSqlite db $ do
    now <- liftIO getCurrentTime
    void $ insert (Member "Marve" "Fleksnes" "marve@fleksnes.no" "secret" now)
    void $ insertMany
        [ Member "Jonas" "Juselius" "jonas.juselius@uit.no" "secret" now
        , Member "Jonas" "Juselius" "jonas@juselius.com" "secret" now ]

getMemberByEmail :: Text -> IO (Maybe Member)
getMemberByEmail e = runSqlite db $ do
    member <- getBy $ Email e
    case member of
        Just (Entity _ x) -> return $ Just x
        Nothing           -> return Nothing

addMember :: FirstName -> LastName -> Email -> Passwd -> IO ()
addMember f l e p = runSqlite db $ do
    now <- liftIO getCurrentTime
    void $ insert (Member f l e p now)


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- module Model.Tables (
--       migrateDb
--     , dbName
--     , Member(..)
--     , Event(..)
--     , UniqueEmail(..)
--     ) where
module Model.Tables where
import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Member
  firstName Text
  lastName Text
  email Text
  password Text
  since UTCTime
  UniqueEmail email
  deriving Show

Event
  name Text
  desc Text
  time UTCTime
  deriving Show
|]

dbName :: FilePath
dbName = "thug.db"

db :: Text
db = pack dbName

migrateDb :: IO ()
migrateDb = runSqlite (pack dbName) $ runMigration migrateAll


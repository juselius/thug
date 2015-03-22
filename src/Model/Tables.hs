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
import Data.Aeson
import Control.Monad
import Control.Applicative

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Member
  firstName Text
  lastName Text
  email Text
  password Text
  joined UTCTime
  UniqueEmail email
  deriving Show

Event
  name Text
  desc Text
  time UTCTime
  deriving Show
|]

instance FromJSON Member where
    parseJSON (Object v) = Member <$>
        v .: "firstName" <*>
        v .: "lastName" <*>
        v .: "email" <*>
        v .: "passwd" <*>
        v .:? "joined" .!= (read "1970-01-01 00:00:00 UTC" :: UTCTime)
    parseJSON _ = mzero

dbName :: FilePath
dbName = "thug.db"

db :: Text
db = pack dbName

migrateDb :: IO ()
migrateDb = runSqlite (pack dbName) $ runMigration migrateAll


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Members (
      migrateDb
    , addMember
    ) where
import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Logger
import Data.Text
import Data.Time
import Database.Esqueleto
import Database.Persist hiding ((==.))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Member
  name Text
  email Text
  password Text
  deriving Show

|]

migrateDb :: IO ()
migrateDb = runSqlite ":memory:" $ do
  runMigration migrateAll

addMember :: IO ()
addMember = runSqlite ":memory:" $ do
  member <- insert (Member "Foo bar" "foo@bar" "secret")
  now <- liftIO getCurrentTime
  -- insertMany
  --   [ StatusUpdate member "Writing another blog post!" now Nothing ]

  -- sortedNames >>= mapM_ (liftIO . print)
  -- latestUpdates >>= mapM_ (liftIO . print)
  return ()

sortedNames =
  select $
  from $ \person -> do
  orderBy [asc (person ^. MemberName)]
  limit 5
  return $ person ^. MemberName


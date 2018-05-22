{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , StandaloneDeriving
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.User where

import LocalCooking.Common.Password (HashedPassword)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Time (UTCTime)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Class (PersistEntity (EntityField, Key), PersistCore (BackendKey))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    created UTCTime
    email EmailAddress
    password HashedPassword
    deriving Eq Show
|]

instance Hashable (Key User) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField User typ) where
  x == y = case x of
    UserPassword -> case y of
      UserPassword -> True
    UserEmail -> case y of
      UserEmail -> True
    UserId -> case y of
      UserId -> True

instance ToJSON (EntityField User typ) where
  toJSON x = case x of
    UserPassword -> String "password"
    UserEmail -> String "email"
    UserId -> String "userId"

instance FromJSON (EntityField User HashedPassword) where
  parseJSON json = case json of
    String s
      | s == "password" -> pure UserPassword
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField User EmailAddress) where
  parseJSON json = case json of
    String s
      | s == "email" -> pure UserEmail
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField User (Key User)) where
  parseJSON json = case json of
    String s
      | s == "userId" -> pure UserId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

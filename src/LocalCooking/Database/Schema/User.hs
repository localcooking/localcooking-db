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

import LocalCooking.Common.User.Password (HashedPassword)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Time (UTCTime)
import Text.EmailAddress (EmailAddress)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Class (PersistEntity (EntityField, Key), PersistCore (BackendKey))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredUser
    created UTCTime
    email EmailAddress
    password HashedPassword
    UniqueEmail email
    deriving Eq Show
|]


instance Hashable StoredUserId where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField StoredUser typ) where
  x == y = case x of
    StoredUserCreated -> case y of
      StoredUserCreated -> True
    StoredUserPassword -> case y of
      StoredUserPassword -> True
    StoredUserEmail -> case y of
      StoredUserEmail -> True
    StoredUserId -> case y of
      StoredUserId -> True

instance ToJSON (EntityField StoredUser typ) where
  toJSON x = case x of
    StoredUserCreated -> String "created"
    StoredUserPassword -> String "password"
    StoredUserEmail -> String "email"
    StoredUserId -> String "storedUserId"

instance FromJSON (EntityField StoredUser UTCTime) where
  parseJSON json = case json of
    String s
      | s == "created" -> pure StoredUserCreated
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json

instance FromJSON (EntityField StoredUser HashedPassword) where
  parseJSON json = case json of
    String s
      | s == "password" -> pure StoredUserPassword
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json

instance FromJSON (EntityField StoredUser EmailAddress) where
  parseJSON json = case json of
    String s
      | s == "email" -> pure StoredUserEmail
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json

instance FromJSON (EntityField StoredUser StoredUserId) where
  parseJSON json = case json of
    String s
      | s == "storedUserId" -> pure StoredUserId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json

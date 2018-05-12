{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.User.Role where

import LocalCooking.Database.Schema.User.Password (UserId)
import LocalCooking.Common.User.Role (UserRole)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserRoleStored
    userRole UserRole
    userRoleOwner UserId
    UniqueUserRole userRole userRoleOwner
    deriving Eq Show
|]

instance Hashable (Key UserRoleStored) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField UserRoleStored typ) where
  x == y = case x of
    UserRoleStoredUserRole -> case y of
      UserRoleStoredUserRole -> True
    UserRoleStoredUserRoleOwner -> case y of
      UserRoleStoredUserRoleOwner -> True
    UserRoleStoredId -> case y of
      UserRoleStoredId -> True

instance ToJSON (EntityField UserRoleStored typ) where
  toJSON x = case x of
    UserRoleStoredUserRole -> String "userRole"
    UserRoleStoredUserRoleOwner -> String "userRoleOwner"
    UserRoleStoredId -> String "userRoleStoredId"

instance FromJSON (EntityField UserRoleStored UserRole) where
  parseJSON json = case json of
    String s
      | s == "userRole" -> pure UserRoleStoredUserRole
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField UserRoleStored UserId) where
  parseJSON json = case json of
    String s
      | s == "userRoleOwner" -> pure UserRoleStoredUserRoleOwner
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField UserRoleStored (Key UserRoleStored)) where
  parseJSON json = case json of
    String s
      | s == "userRoleStoredId" -> pure UserRoleStoredId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

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

module LocalCooking.Database.Schema.User.Pending where

import LocalCooking.Database.Schema.User (UserId)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PendingRegistrationConfirm
    pendingRegister UserId
    UniquePendingRegistration pendingRegister
    deriving Eq Show
|]

instance Hashable (Key PendingRegistrationConfirm) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField PendingRegistrationConfirm typ) where
  x == y = case x of
    PendingRegistrationConfirmPendingRegister -> case y of
      PendingRegistrationConfirmPendingRegister -> True
    PendingRegistrationConfirmId -> case y of
      PendingRegistrationConfirmId -> True

instance ToJSON (EntityField PendingRegistrationConfirm typ) where
  toJSON x = case x of
    PendingRegistrationConfirmPendingRegister -> String "pendingRegister"
    PendingRegistrationConfirmId -> String "pendingRegisterConfirmId"

instance FromJSON (EntityField PendingRegistrationConfirm UserId) where
  parseJSON json = case json of
    String s
      | s == "pendingRegister" -> pure PendingRegistrationConfirmPendingRegister
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField PendingRegistrationConfirm (Key PendingRegistrationConfirm)) where
  parseJSON json = case json of
    String s
      | s == "pendingRegisterConfirmId" -> pure PendingRegistrationConfirmId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

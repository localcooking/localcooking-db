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

module LocalCooking.Database.Schema.User.Email where

import LocalCooking.Database.Schema.User.Password (UserId)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Text.EmailAddress (EmailAddress)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
EmailAddressStored
    emailAddress EmailAddress
    emailAddressOwner UserId
    UniqueEmailAddress emailAddress
    EmailAddressOwner emailAddressOwner
    deriving Eq Show
|]

instance Hashable (Key EmailAddressStored) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField EmailAddressStored typ) where
  x == y = case x of
    EmailAddressStoredEmailAddress -> case y of
      EmailAddressStoredEmailAddress -> True
    EmailAddressStoredEmailAddressOwner -> case y of
      EmailAddressStoredEmailAddressOwner -> True
    EmailAddressStoredId -> case y of
      EmailAddressStoredId -> True

instance ToJSON (EntityField EmailAddressStored typ) where
  toJSON x = case x of
    EmailAddressStoredEmailAddress -> String "address"
    EmailAddressStoredEmailAddressOwner -> String "addressOwner"
    EmailAddressStoredId -> String "emailAddressStoredId"

instance FromJSON (EntityField EmailAddressStored EmailAddress) where
  parseJSON json = case json of
    String s
      | s == "address" -> pure EmailAddressStoredEmailAddress
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField EmailAddressStored" json

instance FromJSON (EntityField EmailAddressStored UserId) where
  parseJSON json = case json of
    String s
      | s == "addressOwner" -> pure EmailAddressStoredEmailAddressOwner
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField EmailAddressStored" json


instance FromJSON (EntityField EmailAddressStored (Key EmailAddressStored)) where
  parseJSON json = case json of
    String s
      | s == "emailAddressStoredId" -> pure EmailAddressStoredId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField EmailAddressStored" json

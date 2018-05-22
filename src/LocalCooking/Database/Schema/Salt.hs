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

module LocalCooking.Database.Schema.Salt where

import LocalCooking.Common.User.Password (HashedPassword)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PasswordSalt
    passwordSalt HashedPassword
    UniquePasswordSalt passwordSalt
    deriving Eq Show
|]

instance Hashable (Key PasswordSalt) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField PasswordSalt typ) where
  x == y = case x of
    PasswordSaltPasswordSalt -> case y of
      PasswordSaltPasswordSalt -> True
    PasswordSaltId -> case y of
      PasswordSaltId -> True

instance ToJSON (EntityField PasswordSalt typ) where
  toJSON x = case x of
    PasswordSaltPasswordSalt -> String "passwordSalt"
    PasswordSaltId -> String "passwordSaltId"

instance FromJSON (EntityField PasswordSalt HashedPassword) where
  parseJSON json = case json of
    String s
      | s == "passwordSalt" -> pure PasswordSaltPasswordSalt
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField PasswordSalt (Key PasswordSalt)) where
  parseJSON json = case json of
    String s
      | s == "passwordSaltId" -> pure PasswordSaltId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

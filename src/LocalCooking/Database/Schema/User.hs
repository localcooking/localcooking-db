{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , StandaloneDeriving
  , MultiParamTypeClasses
  , PartialTypeSignatures
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.User where

import LocalCooking.Common.User.Password (HashedPassword)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Time (UTCTime)
import Text.EmailAddress (EmailAddress)
import Database.Persist.Class (PersistEntity (EntityField))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Test.QuickCheck (Arbitrary (..))
import Unsafe.Coerce (unsafeCoerce)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredUser
    created UTCTime
    email EmailAddress
    password HashedPassword
    confirmed Bool
    UniqueEmail email
    deriving Eq Show
|]


instance Arbitrary StoredUserId where
  arbitrary = unsafeCoerce <$> (arbitrary :: _ Int)

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
    StoredUserConfirmed -> case y of
      StoredUserConfirmed -> True
    StoredUserId -> case y of
      StoredUserId -> True

instance ToJSON (EntityField StoredUser typ) where
  toJSON x = case x of
    StoredUserCreated -> String "created"
    StoredUserPassword -> String "password"
    StoredUserEmail -> String "email"
    StoredUserConfirmed -> String "confirmed"
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

instance FromJSON (EntityField StoredUser Bool) where
  parseJSON json = case json of
    String s
      | s == "confirmed" -> pure StoredUserConfirmed
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

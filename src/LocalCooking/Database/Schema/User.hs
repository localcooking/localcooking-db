{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.User where

import LocalCooking.Common.Password (HashedPassword)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Text.EmailAddress (EmailAddress)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    password HashedPassword
    deriving Eq Show

EmailAddressStored
    emailAddress EmailAddress
    emailAddressOwner UserId
    UniqueEmailAddress emailAddress
    EmailAddressOwner emailAddressOwner
    deriving Eq Show

PendingRegistrationConfirm
    pendingRegister UserId
    UniquePendingRegistration pendingRegister
    deriving Eq Show
|]

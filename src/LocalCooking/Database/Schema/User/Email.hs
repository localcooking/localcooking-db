{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.User.Email where

import LocalCooking.Database.Schema.User.Password (UserId)

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

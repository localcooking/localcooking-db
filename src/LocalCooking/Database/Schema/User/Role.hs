{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.User.Role where

import LocalCooking.Database.Schema.User.Password (UserId)
import LocalCooking.Common.User.Role (UserRole)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserRoleStored
    userRole UserRole
    userRoleOwner UserId
    UniqueUserRole userRole userRoleOwner
    deriving Eq Show
|]

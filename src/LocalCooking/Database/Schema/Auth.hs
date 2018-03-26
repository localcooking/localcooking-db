{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Auth where

import LocalCooking.Database.Schema.Device (RegisteredDeviceTokenId)
import LocalCooking.Database.Schema.User (UserId)
import LocalCooking.Common.AuthToken (AuthToken)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
RegisteredAuthToken
    authToken AuthToken
    authTokenDevice RegisteredDeviceTokenId
    authTokenOwner UserId
    UniqueAuthToken authToken
    AuthTokenDevice authTokenDevice
    AuthTokenOwner authTokenOwner
    deriving Eq Show
|]

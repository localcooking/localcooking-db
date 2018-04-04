{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Auth where

import LocalCooking.Database.Schema.User (UserId)
import LocalCooking.Common.AuthToken (AuthToken)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- acknowledged AuthTokens as bona-fide
RegisteredAuthToken
    authToken AuthToken
    authTokenOwner UserId
    UniqueAuthToken authToken
    deriving Eq Show
|]

{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Facebook.AccessToken where

import LocalCooking.Database.Schema.Facebook.UserDetails (FacebookUserDetailsId)
import Facebook.Types (FacebookUserAccessToken)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FacebookUserAccessTokenStored
    facebookUserAccessToken FacebookUserAccessToken
    facebookUserDetails FacebookUserDetailsId
    UniqueFacebookUserAccessToken facebookUserAccessToken
    FacebookUserAccessTokenOwner facebookUserDetails
    deriving Eq Show
|]

{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Facebook where

import Facebook.User (FacebookUserAccessToken, FacebookUserId)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FacebookUserAccessTokenStored
    facebookUserAccessToken FacebookUserAccessToken
    facebookUserDetails FacebookUserDetailsId
    UniqueFacebookUserAccessToken facebookUserAccessToken
    FacebookUserAccessTokenOwner facebookUserDetails
    deriving Eq Show

FacebookUserDetails
    facebookUserId FacebookUserId
    UniqueFacebookUserID facebookUserId
    deriving Eq Show
|]

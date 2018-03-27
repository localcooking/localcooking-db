{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Facebook where

import LocalCooking.Database.Schema.User (UserId)
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
    facebookUserOwner UserId
    UniqueFacebookUserId facebookUserId
    FacebookUserDetailsOwner facebookUserOwner
    deriving Eq Show
|]

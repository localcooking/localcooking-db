{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Facebook.UserDetails where

import LocalCooking.Database.Schema.User (UserId)
import Facebook.Types (FacebookUserId)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FacebookUserDetails
    facebookUserId FacebookUserId
    facebookUserOwner UserId
    UniqueFacebookUserId facebookUserId
    FacebookUserDetailsOwner facebookUserOwner
    deriving Eq Show
|]

{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database where

import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.DeviceToken (DeviceToken)
import LocalCooking.Common.AuthToken (AuthToken)
import Facebook.User (FacebookUserAccessToken, FacebookUserId)

import Data.Text (Text)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Text.EmailAddress (EmailAddress)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DeviceDescription
    deviceName Text
    deviceToken DeviceToken
    RegisteredDeviceToken deviceToken
    deriving Show

FacebookUserAccessTokenStored
    accessToken FacebookUserAccessToken
    UniqueFacebookUserAccessToken accessToken
    deriving Show

FacebookUserDetails
    facebookUserId FacebookUserId
    facebookSession FacebookUserAccessTokenStoredId []
    UniqueFacebookUserID facebookUserId
    deriving Show

User
    email EmailAddress
    password HashedPassword
    facebookUser FacebookUserDetailsId Maybe
    UserEmailAddress email
    deriving Show
|]

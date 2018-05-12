{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Facebook.AccessToken where

import LocalCooking.Database.Schema.Facebook.UserDetails (FacebookUserDetailsId)
import Facebook.Types (FacebookUserAccessToken)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FacebookUserAccessTokenStored
    facebookUserAccessToken FacebookUserAccessToken
    facebookUserDetails FacebookUserDetailsId
    UniqueFacebookUserAccessToken facebookUserAccessToken
    FacebookUserAccessTokenOwner facebookUserDetails
    deriving Eq Show
|]

instance Hashable (Key FacebookUserAccessTokenStored) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField FacebookUserAccessTokenStored typ) where
  x == y = case x of
    FacebookUserAccessTokenStoredFacebookUserAccessToken -> case y of
      FacebookUserAccessTokenStoredFacebookUserAccessToken -> True
    FacebookUserAccessTokenStoredFacebookUserDetails -> case y of
      FacebookUserAccessTokenStoredFacebookUserDetails -> True
    FacebookUserAccessTokenStoredId -> case y of
      FacebookUserAccessTokenStoredId -> True

instance ToJSON (EntityField FacebookUserAccessTokenStored typ) where
  toJSON x = case x of
    FacebookUserAccessTokenStoredFacebookUserAccessToken -> String "facebookUserAccessToken"
    FacebookUserAccessTokenStoredFacebookUserDetails -> String "facebookUserDetails"
    FacebookUserAccessTokenStoredId -> String "facebookUserAccessTokenStoredId"

instance FromJSON (EntityField FacebookUserAccessTokenStored FacebookUserAccessToken) where
  parseJSON json = case json of
    String s
      | s == "facebookUserAccessToken" -> pure FacebookUserAccessTokenStoredFacebookUserAccessToken
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserAccessTokenStored FacebookUserDetailsId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserDetails" -> pure FacebookUserAccessTokenStoredFacebookUserDetails
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserAccessTokenStored (Key FacebookUserAccessTokenStored)) where
  parseJSON json = case json of
    String s
      | s == "facebookUserAccessTokenStoredId" -> pure FacebookUserAccessTokenStoredId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

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

module LocalCooking.Database.Schema.Facebook.UserDetails where

import LocalCooking.Database.Schema.User (UserId)
import Facebook.Types (FacebookUserId)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FacebookUserDetails
    facebookUserId FacebookUserId
    facebookUserOwner UserId
    UniqueFacebookUserId facebookUserId
    FacebookUserDetailsOwner facebookUserOwner
    deriving Eq Show
|]

instance Hashable (Key FacebookUserDetails) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField FacebookUserDetails typ) where
  x == y = case x of
    FacebookUserDetailsFacebookUserId -> case y of
      FacebookUserDetailsFacebookUserId -> True
    FacebookUserDetailsFacebookUserOwner -> case y of
      FacebookUserDetailsFacebookUserOwner -> True
    FacebookUserDetailsId -> case y of
      FacebookUserDetailsId -> True

instance ToJSON (EntityField FacebookUserDetails typ) where
  toJSON x = case x of
    FacebookUserDetailsFacebookUserId -> String "facebookUserId"
    FacebookUserDetailsFacebookUserOwner -> String "facebookUserOwner"
    FacebookUserDetailsId -> String "facebookUserDetailsId"

instance FromJSON (EntityField FacebookUserDetails FacebookUserId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserId" -> pure FacebookUserDetailsFacebookUserId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserDetails UserId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserOwner" -> pure FacebookUserDetailsFacebookUserOwner
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserDetails (Key FacebookUserDetails)) where
  parseJSON json = case json of
    String s
      | s == "facebookUserDetailsId" -> pure FacebookUserDetailsId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json


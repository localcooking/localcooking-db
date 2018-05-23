{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Admin where

import LocalCooking.Semantics.Common (User, Register)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()



data GetUsers = GetUsers
  deriving (Eq, Show, Generic)

instance Arbitrary GetUsers where
  arbitrary = pure GetUsers

instance ToJSON GetUsers where
  toJSON GetUsers = String "getUsers"

instance FromJSON GetUsers where
  parseJSON json = case json of
    String x
      | x == "getUsers" -> pure GetUsers
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "GetUsers" json



newtype SetUser = SetUser
  { getSetUser :: User
  } deriving (Eq, Show, Generic)

instance Arbitrary SetUser where
  arbitrary = SetUser <$> arbitrary

instance ToJSON SetUser where
  toJSON (SetUser u) = object
    [ "setUser" .= u ]

instance FromJSON SetUser where
  parseJSON json = case json of
    Object o -> SetUser <$> o .: "setUser"
    _ -> fail'
    where
      fail' = typeMismatch "SetUser" json

newtype AddUser = AddUser
  { getAddUser :: Register
  } deriving (Eq, Show, Generic)

instance Arbitrary AddUser where
  arbitrary = AddUser <$> arbitrary

instance ToJSON AddUser where
  toJSON (AddUser u) = object
    [ "addUser" .= u ]

instance FromJSON AddUser where
  parseJSON json = case json of
    Object o -> AddUser <$> o .: "addUser"
    _ -> fail'
    where
      fail' = typeMismatch "AddUser" json

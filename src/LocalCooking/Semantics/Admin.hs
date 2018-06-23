{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Admin where

import LocalCooking.Semantics.Common (User)
import LocalCooking.Common.User.Password (HashedPassword)

import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))
import Text.EmailAddress (EmailAddress)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()



data SetUser
  = SetUserUpdate
    { setUserUpdateUser :: User
    , setUserUpdateNewPassword :: Maybe HashedPassword
    }
  | SetUserDelete User
  deriving (Eq, Show, Generic)

instance Arbitrary SetUser where
  arbitrary = oneof
    [ SetUserUpdate <$> arbitrary <*> arbitrary
    , SetUserDelete <$> arbitrary
    ]

instance ToJSON SetUser where
  toJSON x = case x of
    SetUserUpdate{..} -> object
      [ "setUserUpdate" .= object
        [ "user" .= setUserUpdateUser
        , "newPassword" .= setUserUpdateNewPassword
        ]
      ]
    SetUserDelete user -> object
      [ "setUserDelete" .= object
        [ "user" .= user
        ]
      ]

instance FromJSON SetUser where
  parseJSON json = case json of
    Object o -> do
      let update = do
            o' <- o .: "setUserUpdate"
            SetUserUpdate <$> o' .: "user" <*> o' .: "newPassword"
          delete = do
            o' <- o .: "setUserDelete"
            SetUserDelete <$> o' .: "user"
      update <|> delete
    _ -> typeMismatch "SetUser" json


data NewUser = NewUser
  { newUserEmail :: EmailAddress
  , newUserPassword :: HashedPassword
  } deriving (Eq, Show, Generic)

instance Arbitrary NewUser where
  arbitrary = NewUser <$> arbitrary <*> arbitrary

instance ToJSON NewUser where
  toJSON NewUser{..} = object
    [ "email" .= newUserEmail
    , "password" .= newUserPassword
    ]

instance FromJSON NewUser where
  parseJSON json = case json of
    Object o -> NewUser <$> o .: "email" <*> o .: "password"
    _ -> typeMismatch "NewUser" json


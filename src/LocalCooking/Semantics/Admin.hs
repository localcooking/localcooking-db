{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Admin where

import LocalCooking.Semantics.Common (User)
-- import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Common.User.Password (HashedPassword)
-- import LocalCooking.Common.User.Role (UserRole)
-- import Facebook.Types (FacebookLoginCode, FacebookUserId)
-- import Google.Keys (ReCaptchaResponse)

import Data.Time (UTCTime)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.Encoding as LT
import Control.Applicative ((<|>))
import Text.EmailAddress (EmailAddress)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()



data SetUser = SetUser
  { setUserUser :: User
  , setUserNewPassword :: Maybe HashedPassword
  } deriving (Eq, Show, Generic)

instance Arbitrary SetUser where
  arbitrary = SetUser <$> arbitrary <*> arbitrary

instance ToJSON SetUser where
  toJSON SetUser{..} = object
    [ "user" .= setUserUser
    , "newPassword" .= setUserNewPassword
    ]

instance FromJSON SetUser where
  parseJSON json = case json of
    Object o -> SetUser <$> o .: "user" <*> o .: "newPassword"
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


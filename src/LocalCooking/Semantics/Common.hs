{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Common where

import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole)
import Facebook.Types (FacebookLoginCode, FacebookUserId)
import Google.Keys (ReCaptchaResponse)

import Data.Time (UTCTime)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Text.EmailAddress (EmailAddress)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)



-- | For supplying social login userId's to be recognized by LocalCooking -
-- used by user settings and register
data SocialLoginForm = SocialLoginForm
  { socialLoginFormFb :: Maybe FacebookUserId
  } deriving (Eq, Show, Generic)

instance Arbitrary SocialLoginForm where
  arbitrary = SocialLoginForm <$> arbitrary

instance ToJSON SocialLoginForm where
  toJSON SocialLoginForm{..} = object
    [ "fb" .= socialLoginFormFb
    ]

instance FromJSON SocialLoginForm where
  parseJSON json = case json of
    Object o -> SocialLoginForm <$> o .: "fb"
    _ -> typeMismatch "SocialLoginForm" json


-- | How a user sees themselves, across all apps - i.e. the result of a login. Roles are only visible to Admin,
--   while UserDetails dictate the availability of those roles
data User = User
  { userId             :: StoredUserId
  , userCreated        :: UTCTime
  , userEmail          :: EmailAddress
  , userSocial         :: SocialLoginForm
  , userEmailConfirmed :: Bool
  , userRoles          :: [UserRole]
  } deriving (Eq, Show, Generic)

instance Arbitrary User where
  arbitrary = User <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON User where
  toJSON User{..} = object
    [ "id" .= userId
    , "created" .= userCreated
    , "email" .= userEmail
    , "social" .= userSocial
    , "emailConfirmed" .= userEmailConfirmed
    , "userRoles" .= userRoles
    ]

instance FromJSON User where
  parseJSON json = case json of
    Object o -> User <$> o .: "id"
                     <*> o .: "created"
                     <*> o .: "email"
                     <*> o .: "social"
                     <*> o .: "emailConfirmed"
                     <*> o .: "userRoles"
    _ -> typeMismatch "User" json



data Register = Register
  { registerEmail     :: EmailAddress
  , registerPassword  :: HashedPassword
  , registerSocial    :: SocialLoginForm
  , registerReCaptcha :: ReCaptchaResponse
  } deriving (Eq, Show, Generic)

instance Arbitrary Register where
  arbitrary = Register <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

instance ToJSON Register where
  toJSON Register{..} = object
    [ "email" .= registerEmail
    , "password" .= registerPassword
    , "social" .= registerSocial
    , "reCaptcha" .= registerReCaptcha
    ]

instance FromJSON Register where
  parseJSON json = case json of
    Object o -> Register <$> o .: "email"
                         <*> o .: "password"
                         <*> o .: "social"
                         <*> o .: "reCaptcha"
    _ -> typeMismatch "Register" json


data Login = Login
  { loginEmail    :: EmailAddress
  , loginPassword :: HashedPassword
  } deriving (Eq, Show, Generic)

instance Arbitrary Login where
  arbitrary = Login <$> arbitrary
                    <*> arbitrary

instance ToJSON Login where
  toJSON Login{..} = object
    [ "email" .= loginEmail
    , "password" .= loginPassword
    ]

instance FromJSON Login where
  parseJSON json = case json of
    Object o -> Login <$> o .: "email"
                      <*> o .: "password"
    _ -> typeMismatch "Login" json


data SocialLogin
  = SocialLoginFB
    { socialLoginFBCode :: FacebookLoginCode
    }
  deriving (Eq, Show, Generic)

instance Arbitrary SocialLogin where
  arbitrary = oneof
    [ SocialLoginFB <$> arbitrary
    ]

instance ToJSON SocialLogin where
  toJSON x = case x of
    SocialLoginFB{..} -> object
      [ "fbCode" .= socialLoginFBCode
      ]

instance FromJSON SocialLogin where
  parseJSON json = case json of
    Object o ->
      let fb = SocialLoginFB <$> o .: "fbCode"
      in  fb
    _ -> typeMismatch "SocialLogin" json

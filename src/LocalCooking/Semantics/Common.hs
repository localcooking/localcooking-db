{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Common where

import LocalCooking.Database.Schema (StoredUserId)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole)
import Facebook.Types (FacebookLoginCode, FacebookUserId)
import Google.Keys (ReCaptchaResponse)

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
  , userSocialLogin    :: SocialLoginForm
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
    , "socialLogin" .= userSocialLogin
    , "emailConfirmed" .= userEmailConfirmed
    , "roles" .= userRoles
    ]

instance FromJSON User where
  parseJSON json = case json of
    Object o -> User <$> o .: "id"
                     <*> o .: "created"
                     <*> o .: "email"
                     <*> o .: "socialLogin"
                     <*> o .: "emailConfirmed"
                     <*> o .: "roles"
    _ -> typeMismatch "User" json

data SetUser = SetUser
  { setUserId          :: StoredUserId
  , setUserEmail       :: EmailAddress
  , setUserSocialLogin :: SocialLoginForm
  , setUserOldPassword :: HashedPassword
  , setUserNewPassword :: HashedPassword
  } deriving (Eq, Show, Generic)

instance Arbitrary SetUser where
  arbitrary = SetUser <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance ToJSON SetUser where
  toJSON SetUser{..} = object
    [ "id" .= setUserId
    , "email" .= setUserEmail
    , "socialLogin" .= setUserSocialLogin
    , "oldPassword" .= setUserOldPassword
    , "newPassword" .= setUserNewPassword
    ]

instance FromJSON SetUser where
  parseJSON json = case json of
    Object o -> SetUser <$> o .: "id"
                        <*> o .: "email"
                        <*> o .: "socialLogin"
                        <*> o .: "oldPassword"
                        <*> o .: "newPassword"
    _ -> typeMismatch "SetUser" json


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


data RegisterError
  = RegisterDecodingError LBS.ByteString
  | RegisterReCaptchaFailure LBS.ByteString
  | RegisterEmailTaken
  deriving (Eq, Show, Generic)

instance Arbitrary RegisterError where
  arbitrary = oneof
    [ RegisterDecodingError . LT.encodeUtf8 <$> arbitrary
    , RegisterReCaptchaFailure . LT.encodeUtf8 <$> arbitrary
    , pure RegisterEmailTaken
    ]

instance ToJSON RegisterError where
  toJSON x = case x of
    RegisterDecodingError e -> object ["decodingError" .= LT.decodeUtf8 e]
    RegisterReCaptchaFailure e -> object ["reCaptchaFailure" .= LT.decodeUtf8 e]
    RegisterEmailTaken -> String "emailTaken"


instance FromJSON RegisterError where
  parseJSON json = case json of
    Object o -> do
      let decodingError = RegisterDecodingError . LT.encodeUtf8 <$> o .: "decodingError"
          reCaptchaFailure = RegisterReCaptchaFailure . LT.encodeUtf8 <$> o .: "reCaptchaFailure"
      decodingError <|> reCaptchaFailure
    String s
      | s == "emailTaken" -> pure RegisterEmailTaken
      | otherwise         -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "Register" json


data ConfirmEmailError
  = ConfirmEmailTokenNonexistent
  | ConfirmEmailUserNonexistent
  | ConfirmEmailOk
  deriving (Eq, Show, Generic)

instance Arbitrary ConfirmEmailError where
  arbitrary = oneof
    [ pure ConfirmEmailTokenNonexistent
    , pure ConfirmEmailUserNonexistent
    , pure ConfirmEmailOk
    ]

instance ToJSON ConfirmEmailError where
  toJSON x = case x of
    ConfirmEmailTokenNonexistent -> String "tokenNonexistent"
    ConfirmEmailUserNonexistent -> String "userNonexistent"
    ConfirmEmailOk -> String "ok"


instance FromJSON ConfirmEmailError where
  parseJSON json = case json of
    String s
      | s == "tokenNonexistent" -> pure ConfirmEmailTokenNonexistent
      | s == "userNonexistent" -> pure ConfirmEmailUserNonexistent
      | s == "ok"              -> pure ConfirmEmailOk
      | otherwise             -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "ConfirmEmail" json



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
    { socialLoginFB :: FacebookLoginCode
    }
  deriving (Eq, Show, Generic)

instance Arbitrary SocialLogin where
  arbitrary = oneof
    [ SocialLoginFB <$> arbitrary
    ]

instance ToJSON SocialLogin where
  toJSON x = case x of
    SocialLoginFB{..} -> object
      [ "fb" .= socialLoginFB
      ]

instance FromJSON SocialLogin where
  parseJSON json = case json of
    Object o -> SocialLoginFB <$> o .: "fb"
    _ -> typeMismatch "SocialLogin" json



-- | Segregated record key from semantic datum
data WithId k a = WithId
  { withIdId :: k
  , withIdContent :: a
  } deriving (Eq, Show, Generic)

instance (Arbitrary k, Arbitrary a) => Arbitrary (WithId k a) where
  arbitrary = WithId <$> arbitrary <*> arbitrary

instance (ToJSON k, ToJSON a) => ToJSON (WithId k a) where
  toJSON WithId{..} = object
    [ "id" .= withIdId
    , "content" .= withIdContent
    ]

instance (FromJSON k, FromJSON a) => FromJSON (WithId k a) where
  parseJSON json = case json of
    Object o -> WithId <$> o .: "id" <*> o .: "content"
    _ -> typeMismatch "WithId" json


uncurryWithId :: (k -> a -> b) -> WithId k a -> b
uncurryWithId f (WithId k a) = f k a

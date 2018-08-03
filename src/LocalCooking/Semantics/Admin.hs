{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , DeriveFunctor
  #-}

module LocalCooking.Semantics.Admin where

import LocalCooking.Database.Schema (StoredEditorId)
import LocalCooking.Semantics.Common (User)
import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)

import Data.Password (HashedPassword)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object, String), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))
import Text.EmailAddress (EmailAddress)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()



-- | Possible modifications to a User entry
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


-- | Data-view for witnessing an editor's authority over a content variant
data GetSetSubmissionPolicy = GetSetSubmissionPolicy
  { getSetSubmissionPolicyVariant    :: ContentRecordVariant
  , getSetSubmissionPolicyAdditional :: Int
  , getSetSubmissionPolicyAssigned   :: [StoredEditorId]
  } deriving (Eq, Show, Generic)

instance Arbitrary GetSetSubmissionPolicy where
  arbitrary = GetSetSubmissionPolicy <$> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary

instance ToJSON GetSetSubmissionPolicy where
  toJSON GetSetSubmissionPolicy{..} = object
    [ "variant" .= getSetSubmissionPolicyVariant
    , "additional" .= getSetSubmissionPolicyAdditional
    , "assigned" .= getSetSubmissionPolicyAssigned
    ]

instance FromJSON GetSetSubmissionPolicy where
  parseJSON json = case json of
    Object o -> GetSetSubmissionPolicy <$> o .: "variant"
                                       <*> o .: "additional"
                                       <*> o .: "assigned"
    _ -> typeMismatch "GetSetSubmissionPolicy" json


-- * Errors



data SubmissionPolicyUnique a
  = SubmissionPolicyNotUnique
  | SubmissionPolicyUnique a
  deriving (Eq, Show, Generic, Functor)

instance Applicative SubmissionPolicyUnique where
  pure = SubmissionPolicyUnique
  (<*>) f x = case f of
    SubmissionPolicyNotUnique -> SubmissionPolicyNotUnique
    SubmissionPolicyUnique f' -> f' <$> x

instance Monad SubmissionPolicyUnique where
  return = pure
  (>>=) x f = case x of
    SubmissionPolicyNotUnique -> SubmissionPolicyNotUnique
    SubmissionPolicyUnique x' -> f x'

instance Arbitrary a => Arbitrary (SubmissionPolicyUnique a) where
  arbitrary = oneof
    [ pure SubmissionPolicyNotUnique
    , SubmissionPolicyUnique <$> arbitrary
    ]

instance ToJSON a => ToJSON (SubmissionPolicyUnique a) where
  toJSON x = case x of
    SubmissionPolicyNotUnique -> String "submissionPolicyNotUnique"
    SubmissionPolicyUnique a -> object ["submissionPolicyUnique" .= a]

instance FromJSON a => FromJSON (SubmissionPolicyUnique a) where
  parseJSON x = case x of
    String s
      | s == "submissionPolicyNotUnique" -> pure SubmissionPolicyNotUnique
      | otherwise -> fail'
    Object o -> SubmissionPolicyUnique <$> o .: "submissionPolicyUnique"
    _ -> fail'
    where
      fail' = typeMismatch "SubmissionPolicyUnique" x

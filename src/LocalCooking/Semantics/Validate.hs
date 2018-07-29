{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  , DeriveFunctor
  #-}

module LocalCooking.Semantics.Validate where

import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.User.Password (HashedPassword)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Traversable (traverse)
import Data.Foldable (asum)
import qualified Data.Text as T
import Data.Text.Permalink (Permalink)
import Data.Time.Calendar (Day)
import Text.EmailAddress (EmailAddress)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()


data PasswordVerifyUnauth = PasswordVerifyUnauth
  { passwordVerifyUnauthEmail :: EmailAddress
  , passwordVerifyUnauthPassword :: HashedPassword
  } deriving (Eq, Show, Generic)

uncurryPasswordVerifyUnauth :: (EmailAddress -> HashedPassword -> a) -> PasswordVerifyUnauth -> a
uncurryPasswordVerifyUnauth f (PasswordVerifyUnauth a b) = f a b

instance Arbitrary PasswordVerifyUnauth where
  arbitrary = PasswordVerifyUnauth <$> arbitrary <*> arbitrary

instance ToJSON PasswordVerifyUnauth where
  toJSON PasswordVerifyUnauth{..} = object
    [ "email" .= passwordVerifyUnauthEmail
    , "password" .= passwordVerifyUnauthPassword
    ]

instance FromJSON PasswordVerifyUnauth where
  parseJSON json = case json of
    Object o -> PasswordVerifyUnauth <$> o .: "email" <*> o .: "password"
    _ -> typeMismatch "PasswordVerifyUnauth" json


data IsUniqueMealPermalink = IsUniqueMealPermalink
  { uniqueMealPermalinkChef :: Permalink
  , uniqueMealPermalinkDeadline :: Day
  , uniqueMealPermalinkMeal :: Permalink
  } deriving (Eq, Show, Generic)

uncurryUniqueMealPermalink :: (Permalink -> Day -> Permalink -> a) -> IsUniqueMealPermalink -> a
uncurryUniqueMealPermalink f (IsUniqueMealPermalink a b c) = f a b c

instance Arbitrary IsUniqueMealPermalink where
  arbitrary = IsUniqueMealPermalink <$> arbitrary <*> arbitrary <*> arbitrary

instance ToJSON IsUniqueMealPermalink where
  toJSON IsUniqueMealPermalink{..} = object
    [ "chef" .= uniqueMealPermalinkChef
    , "menu" .= uniqueMealPermalinkDeadline
    , "meal" .= uniqueMealPermalinkMeal
    ]

instance FromJSON IsUniqueMealPermalink where
  parseJSON json = case json of
    Object o -> IsUniqueMealPermalink <$> o .: "chef" <*> o .: "menu" <*> o .: "meal"
    _ -> typeMismatch "IsUniqueMealPermalink" json


data IsUniqueMenuDeadline = IsUniqueMenuDeadline
  { uniqueMenuDeadlineChef :: Permalink
  , uniqueMenuDeadlineDeadline :: Day
  } deriving (Eq, Show, Generic)

uncurryUniqueMenuDeadline :: (Permalink -> Day -> a) -> IsUniqueMenuDeadline -> a
uncurryUniqueMenuDeadline f (IsUniqueMenuDeadline a b) = f a b


instance Arbitrary IsUniqueMenuDeadline where
  arbitrary = IsUniqueMenuDeadline <$> arbitrary <*> arbitrary

instance ToJSON IsUniqueMenuDeadline where
  toJSON IsUniqueMenuDeadline{..} = object
    [ "chef" .= uniqueMenuDeadlineChef
    , "menu" .= uniqueMenuDeadlineDeadline
    ]

instance FromJSON IsUniqueMenuDeadline where
  parseJSON json = case json of
    Object o -> IsUniqueMenuDeadline <$> o .: "chef" <*> o .: "menu"
    _ -> typeMismatch "IsUniqueMenuDeadline" json



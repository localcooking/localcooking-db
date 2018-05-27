{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Validate where

import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Text.Permalink (Permalink)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Text.EmailAddress (EmailAddress)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)



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

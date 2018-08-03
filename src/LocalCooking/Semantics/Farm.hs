{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Farm where

import LocalCooking.Common.Tag.Farm (FarmTag)

import Data.Name (Name)
import Data.Price (Price)
import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()



data FarmerSettings = FarmerSettings
  { farmerSettingsName      :: Name
  , farmerSettingsPermalink :: Permalink
  , farmerSettingsImages    :: [ImageSource]
  , farmerSettingsAvatar    :: ImageSource
  , farmerSettingsBio       :: MarkdownText
  , farmerSettingsTags      :: [FarmTag]
  } deriving (Eq, Show, Generic)

instance Arbitrary FarmerSettings where
  arbitrary = FarmerSettings <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON FarmerSettings where
  toJSON FarmerSettings{..} = object
    [ "name" .= farmerSettingsName
    , "permalink" .= farmerSettingsPermalink
    , "images" .= farmerSettingsImages
    , "avatar" .= farmerSettingsAvatar
    , "bio" .= farmerSettingsBio
    , "tags" .= farmerSettingsTags
    ]

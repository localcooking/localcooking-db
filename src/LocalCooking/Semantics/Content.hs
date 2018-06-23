{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Content where

import LocalCooking.Database.Schema.Content (RecordSubmissionApprovalId)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.ContentRecord (ContentRecordVariant)

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


data GetEditor = GetEditor
  { getEditorName :: Name
  , getEditorAssignedRecords :: [ContentRecordVariant]
  , getEditorApprovedSubmissions :: [RecordSubmissionApprovalId]
  } deriving (Eq, Show, Generic)

instance Arbitrary GetEditor where
  arbitrary = GetEditor <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance ToJSON GetEditor where
  toJSON GetEditor{..} = object
    [ "name" .= getEditorName
    , "assignedRecords" .= getEditorAssignedRecords
    , "approvedSubmissions" .= getEditorApprovedSubmissions
    ]

instance FromJSON GetEditor where
  parseJSON json = case json of
    Object o -> GetEditor <$> o .: "name"
                          <*> o .: "assignedRecords"
                          <*> o .: "approvedSubmissions"
    _ -> typeMismatch "GetEditor" json



data SetEditor = SetEditor
  { setEditorName :: Name
  } deriving (Eq, Show, Generic)

instance Arbitrary SetEditor where
  arbitrary = SetEditor <$> arbitrary

instance ToJSON SetEditor where
  toJSON SetEditor{..} = object
    [ "name" .= setEditorName
    ]

instance FromJSON SetEditor where
  parseJSON json = case json of
    Object o -> SetEditor <$> o .: "name"
    _ -> typeMismatch "SetEditor" json

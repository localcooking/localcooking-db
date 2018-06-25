{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.Content where

import LocalCooking.Semantics.ContentRecord (ContentRecordVariant, ContentRecord)
import LocalCooking.Database.Schema (StoredUserId, StoredEditorId)
import LocalCooking.Database.Schema.Content (RecordSubmissionApprovalId)
import LocalCooking.Common.User.Name (Name)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Time (UTCTime)
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



data GetRecordSubmissionPolicy = GetRecordSubmissionPolicy
  { getRecordSubmissionPolicyVariant    :: ContentRecordVariant
  , getRecordSubmissionPolicyAdditional :: Int
  , getRecordSubmissionPolicyAssigned   :: [StoredEditorId]
  } deriving (Eq, Show, Generic)

instance Arbitrary GetRecordSubmissionPolicy where
  arbitrary = GetRecordSubmissionPolicy <$> arbitrary
                                        <*> arbitrary
                                        <*> arbitrary

instance ToJSON GetRecordSubmissionPolicy where
  toJSON GetRecordSubmissionPolicy{..} = object
    [ "variant" .= getRecordSubmissionPolicyVariant
    , "additional" .= getRecordSubmissionPolicyAdditional
    , "assigned" .= getRecordSubmissionPolicyAssigned
    ]

instance FromJSON GetRecordSubmissionPolicy where
  parseJSON json = case json of
    Object o -> GetRecordSubmissionPolicy <$> o .: "variant"
                                          <*> o .: "additional"
                                          <*> o .: "assigned"
    _ -> typeMismatch "GetRecordSubmissionPolicy" json


data GetRecordSubmission = GetRecordSubmission
  { getRecordSubmissionAuthor :: StoredUserId
  , getRecordSubmissionTimestamp :: UTCTime
  , getRecordSubmissionSubmission :: ContentRecord
  , getRecordSubmissionApprovals :: [StoredEditorId]
  } deriving (Eq, Show, Generic)

instance Arbitrary GetRecordSubmission where
  arbitrary = GetRecordSubmission <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary

instance ToJSON GetRecordSubmission where
  toJSON GetRecordSubmission{..} = object
    [ "author" .= getRecordSubmissionAuthor
    , "timestamp" .= getRecordSubmissionTimestamp
    , "submission" .= getRecordSubmissionSubmission
    , "approvals" .= getRecordSubmissionApprovals
    ]

instance FromJSON GetRecordSubmission where
  parseJSON json = case json of
    Object o -> GetRecordSubmission <$> o .: "author"
                                    <*> o .: "timestamp"
                                    <*> o .: "submission"
                                    <*> o .: "approvals"
    _ -> typeMismatch "GetRecordSubmission" json

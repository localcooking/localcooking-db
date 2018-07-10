{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.Content.Approval where

import LocalCooking.Semantics.ContentRecord (ContentRecord)
import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
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

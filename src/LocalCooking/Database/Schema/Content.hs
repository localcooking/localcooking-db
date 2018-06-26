{-# LANGUAGE
    QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , MultiParamTypeClasses
  , ExistentialQuantification
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Content where

import LocalCooking.Database.Schema (StoredEditorId, StoredUserId)
import LocalCooking.Semantics.ContentRecord (ContentRecord, ContentRecordVariant)

import Data.Time (UTCTime)
import Database.Persist.Sql (toSqlKey)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Test.QuickCheck (Arbitrary (..))




share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

-- * Content

FIXME should be total for all content record variants
RecordSubmissionPolicy
    variant ContentRecordVariant
    additional Int
    UniqueSubmissionPolicyVariant variant
    deriving Eq Show

RecordAssignedSubmissionPolicy
    policy RecordSubmissionPolicyId
    editor StoredEditorId
    UniqueAssignedSubmissionPolicy policy editor
    deriving Eq Show

StoredRecordSubmission
    author StoredUserId
    timestamp UTCTime
    record ContentRecord
    variant ContentRecordVariant
    deriving Eq Show

RecordSubmissionApproval
    record StoredRecordSubmissionId
    editor StoredEditorId
    UniqueSubmissionApproval record editor
    deriving Eq Show
|]



instance Arbitrary RecordSubmissionApprovalId where
  arbitrary = toSqlKey <$> arbitrary

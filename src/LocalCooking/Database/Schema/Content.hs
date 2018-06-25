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
import qualified LocalCooking.Database.Schema as Schema
import LocalCooking.Semantics.ContentRecord (ContentRecord, ContentRecordVariant)

import Data.Time (UTCTime)
import Database.Persist.Sql (toSqlKey)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Test.QuickCheck (Arbitrary (..))




share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

-- * Content

FIXME should be total for all content record variants
RecordSubmissionPolicy
    recordSubmissionPolicyVariant ContentRecordVariant
    recordSubmissionPolicyAdditional Int
    UniqueSubmissionPolicyVariant recordSubmissionPolicyVariant
    deriving Eq Show

RecordAssignedSubmissionPolicy
    recordAssignedSubmissionPolicy RecordSubmissionPolicyId
    recordAssignedSubmissionPolicyEditor StoredEditorId
    UniqueAssignedSubmissionPolicy recordAssignedSubmissionPolicy recordAssignedSubmissionPolicyEditor
    deriving Eq Show

StoredRecordSubmission
    storedRecordSubmissionAuthor StoredUserId
    storedRecordSubmissionTimestamp UTCTime
    storedRecordSubmission ContentRecord
    deriving Eq Show

RecordSubmissionApproval
    recordSubmissionApprovalRecord StoredRecordSubmissionId
    recordSubmissionApprovalEditor StoredEditorId
    UniqueSubmissionApproval recordSubmissionApprovalRecord recordSubmissionApprovalEditor
    deriving Eq Show
|]



instance Arbitrary RecordSubmissionApprovalId where
  arbitrary = toSqlKey <$> arbitrary

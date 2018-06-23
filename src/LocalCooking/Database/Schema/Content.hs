{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Content where

import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Schema.User.Editor (StoredEditorId)
import LocalCooking.Common.ContentRecord (ContentRecord, ContentRecordVariant)

import Data.Time (UTCTime)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
RecordSubmissionPolicy
    recordSubmissionPolicyVariant ContentRecordVariant
    recordSubmissionPolicyAdditional Int
    UniquePolicyContentRecordVariant recordSubmissionPolicyVariant
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

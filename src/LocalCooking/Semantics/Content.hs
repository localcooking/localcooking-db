{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.Content where

import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Database.Schema (StoredUserId, StoredEditorId)
import LocalCooking.Common.User.Name (Name)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()



data SetEditor = SetEditor
  { setEditorName :: Maybe Name
  } deriving (Eq, Show, Generic)

emptySetEditor :: SetEditor
emptySetEditor = SetEditor Nothing

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



data EditorValid = EditorValid
  { editorValidName :: Name
  } deriving (Eq, Show, Generic)

instance Arbitrary EditorValid where
  arbitrary = EditorValid <$> arbitrary

instance ToJSON EditorValid where
  toJSON EditorValid{..} = object
    [ "name" .= editorValidName
    ]

instance FromJSON EditorValid where
  parseJSON json = case json of
    Object o -> EditorValid <$> o .: "name"
    _ -> typeMismatch "EditorValid" json



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

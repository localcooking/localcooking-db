{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.Content where

import LocalCooking.Semantics.ContentRecord (ContentRecordVariant)
import LocalCooking.Database.Schema (StoredEditorId)
import LocalCooking.Database.Schema.Content (RecordSubmissionApprovalId)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Culture (CultureTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Meal (MealTag)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Control.Applicative ((<|>))
import Database.Persist.TH (derivePersistFieldJSON)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
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



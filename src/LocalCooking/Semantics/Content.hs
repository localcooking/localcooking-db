{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , DeriveFunctor
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.Content where

import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Database.Schema (StoredEditorId)
import LocalCooking.Common.User.Name (Name)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
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


-- * Errors

data SubmissionPolicy a
  = NoSubmissionPolicy
  | SubmissionPolicy a
  deriving (Eq, Show, Generic, Functor)

instance Applicative SubmissionPolicy where
  pure = SubmissionPolicy
  (<*>) f x = case f of
    NoSubmissionPolicy -> NoSubmissionPolicy
    SubmissionPolicy f' -> f' <$> x

instance Monad SubmissionPolicy where
  return = pure
  (>>=) x f = case x of
    NoSubmissionPolicy -> NoSubmissionPolicy
    SubmissionPolicy x' -> f x'

instance Arbitrary a => Arbitrary (SubmissionPolicy a) where
  arbitrary = oneof
    [ pure NoSubmissionPolicy
    , SubmissionPolicy <$> arbitrary
    ]

instance ToJSON a => ToJSON (SubmissionPolicy a) where
  toJSON x = case x of
    NoSubmissionPolicy -> String "noSubmissionPolicy"
    SubmissionPolicy a -> object ["submissionPolicy" .= a]

instance FromJSON a => FromJSON (SubmissionPolicy a) where
  parseJSON x = case x of
    String s
      | s == "noSubmissionPolicy" -> pure NoSubmissionPolicy
      | otherwise -> fail'
    Object o -> SubmissionPolicy <$> o .: "submissionPolicy"
    _ -> fail'
    where
      fail' = typeMismatch "SubmissionPolicy" x


data SubmissionExists a
  = SubmissionDoesntExist
  | SubmissionExists a
  deriving (Eq, Show, Generic, Functor)

instance Applicative SubmissionExists where
  pure = SubmissionExists
  (<*>) f x = case f of
    SubmissionDoesntExist -> SubmissionDoesntExist
    SubmissionExists f' -> f' <$> x

instance Monad SubmissionExists where
  return = pure
  (>>=) x f = case x of
    SubmissionDoesntExist -> SubmissionDoesntExist
    SubmissionExists x' -> f x'

instance Arbitrary a => Arbitrary (SubmissionExists a) where
  arbitrary = oneof
    [ pure SubmissionDoesntExist
    , SubmissionExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (SubmissionExists a) where
  toJSON x = case x of
    SubmissionDoesntExist -> String "submissionDoesntExist"
    SubmissionExists a -> object ["submissionExists" .= a]

instance FromJSON a => FromJSON (SubmissionExists a) where
  parseJSON x = case x of
    String s
      | s == "submissionDoesntExist" -> pure SubmissionDoesntExist
      | otherwise -> fail'
    Object o -> SubmissionExists <$> o .: "submissionExists"
    _ -> fail'
    where
      fail' = typeMismatch "SubmissionExists" x


data EditorExists a
  = EditorDoesntExist
  | EditorExists a
  deriving (Eq, Show, Generic, Functor)

instance Applicative EditorExists where
  pure = EditorExists
  (<*>) f x = case f of
    EditorDoesntExist -> EditorDoesntExist
    EditorExists f' -> f' <$> x

instance Monad EditorExists where
  return = pure
  (>>=) x f = case x of
    EditorDoesntExist -> EditorDoesntExist
    EditorExists x' -> f x'

instance Arbitrary a => Arbitrary (EditorExists a) where
  arbitrary = oneof
    [ pure EditorDoesntExist
    , EditorExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (EditorExists a) where
  toJSON x = case x of
    EditorDoesntExist -> String "editorDoesntExist"
    EditorExists a -> object ["editorExists" .= a]

instance FromJSON a => FromJSON (EditorExists a) where
  parseJSON x = case x of
    String s
      | s == "editorDoesntExist" -> pure EditorDoesntExist
      | otherwise -> fail'
    Object o -> EditorExists <$> o .: "editorExists"
    _ -> fail'
    where
      fail' = typeMismatch "EditorExists" x

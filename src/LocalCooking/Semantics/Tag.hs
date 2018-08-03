{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , DeriveFunctor
  #-}

module LocalCooking.Semantics.Tag where

import LocalCooking.Database.Schema (StoredBlogPostId, StoredBlogPostCategoryId)
import LocalCooking.Common.Blog (BlogPostVariant, BlogPostCategory, BlogPostPriority)

import Data.Name (Name)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()



-- * Errors



data TagExists a
  = TagDoesntExist
  | TagExists a
  deriving (Eq, Show, Generic, Functor)

tagExistsToMaybe :: TagExists a -> Maybe a
tagExistsToMaybe x = case x of
  TagDoesntExist -> Nothing
  TagExists y -> Just y

instance Applicative TagExists where
  pure = TagExists
  (<*>) f x = case f of
    TagDoesntExist -> TagDoesntExist
    TagExists f' -> f' <$> x

instance Monad TagExists where
  return = pure
  (>>=) x f = case x of
    TagDoesntExist -> TagDoesntExist
    TagExists x' -> f x'

instance Arbitrary a => Arbitrary (TagExists a) where
  arbitrary = oneof
    [ pure TagDoesntExist
    , TagExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (TagExists a) where
  toJSON x = case x of
    TagDoesntExist -> String "tagDoesntExist"
    TagExists a -> object ["tagExists" .= a]

instance FromJSON a => FromJSON (TagExists a) where
  parseJSON x = case x of
    String s
      | s == "tagDoesntExist" -> pure TagDoesntExist
      | otherwise -> fail'
    Object o -> TagExists <$> o .: "tagExists"
    _ -> fail'
    where
      fail' = typeMismatch "TagExists" x

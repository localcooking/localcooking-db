{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Blog where

import LocalCooking.Database.Schema (StoredUserId)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Order (OrderProgress)

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



data BlogPostSynopsis = BlogPostSynopsis
  { blogPostSynopsisAuthor    :: Name
  , blogPostSynopsisTimestamp :: UTCTime
  , blogPostSynopsisHeadline  :: Text
  , blogPostSynopsisPermalink :: Permalink
  } deriving (Eq, Show, Generic)


instance Arbitrary BlogPostSynopsis where
  arbitrary = BlogPostSynopsis <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

instance ToJSON BlogPostSynopsis where
  toJSON BlogPostSynopsis{..} = object
    [ "author" .= blogPostSynopsisAuthor
    , "timestamp" .= blogPostSynopsisTimestamp
    , "headline" .= blogPostSynopsisHeadline
    , "permalink" .= blogPostSynopsisPermalink
    ]

instance FromJSON BlogPostSynopsis where
  parseJSON json = case json of
    Object o -> BlogPostSynopsis <$> o .: "author"
                                  <*> o .: "timestamp"
                                  <*> o .: "headline"
                                  <*> o .: "permalink"
    _ -> typeMismatch "BlogPostSynopsis" json


data GetBlogPost = GetBlogPost
  { getBlogPostAuthor    :: Name
  , getBlogPostTimestamp :: UTCTime
  , getBlogPostHeadline  :: Text
  , getBlogPostPermalink :: Permalink
  , getBlogPostContent   :: MarkdownText
  } deriving (Eq, Show, Generic)


instance Arbitrary GetBlogPost where
  arbitrary = GetBlogPost <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance ToJSON GetBlogPost where
  toJSON GetBlogPost{..} = object
    [ "author" .= getBlogPostAuthor
    , "timestamp" .= getBlogPostTimestamp
    , "headline" .= getBlogPostHeadline
    , "permalink" .= getBlogPostPermalink
    , "content" .= getBlogPostContent
    ]

instance FromJSON GetBlogPost where
  parseJSON json = case json of
    Object o -> GetBlogPost <$> o .: "author"
                            <*> o .: "timestamp"
                            <*> o .: "headline"
                            <*> o .: "permalink"
                            <*> o .: "content"
    _ -> typeMismatch "GetBlogPost" json


data NewBlogPost = NewBlogPost
  { newBlogPostHeadline  :: Text
  , newBlogPostPermalink :: Permalink
  , newBlogPostContent   :: MarkdownText
  } deriving (Eq, Show, Generic)


instance Arbitrary NewBlogPost where
  arbitrary = NewBlogPost <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance ToJSON NewBlogPost where
  toJSON NewBlogPost{..} = object
    [ "headline" .= newBlogPostHeadline
    , "permalink" .= newBlogPostPermalink
    , "content" .= newBlogPostContent
    ]

instance FromJSON NewBlogPost where
  parseJSON json = case json of
    Object o -> NewBlogPost <$> o .: "headline"
                            <*> o .: "permalink"
                            <*> o .: "content"
    _ -> typeMismatch "NewBlogPost" json


data SetBlogPost = SetBlogPost
  { setBlogPostHeadline  :: Text
  , setBlogPostPermalink :: Permalink
  , setBlogPostContent   :: MarkdownText
  } deriving (Eq, Show, Generic)


instance Arbitrary SetBlogPost where
  arbitrary = SetBlogPost <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance ToJSON SetBlogPost where
  toJSON SetBlogPost{..} = object
    [ "headline" .= setBlogPostHeadline
    , "permalink" .= setBlogPostPermalink
    , "content" .= setBlogPostContent
    ]

instance FromJSON SetBlogPost where
  parseJSON json = case json of
    Object o -> SetBlogPost <$> o .: "headline"
                            <*> o .: "permalink"
                            <*> o .: "content"
    _ -> typeMismatch "SetBlogPost" json

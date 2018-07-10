{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Blog where

import LocalCooking.Database.Schema (StoredUserId, StoredBlogPostId, StoredBlogPostCategoryId)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Blog (BlogPostVariant, BlogPostCategory, BlogPostPriority)

import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()



-- * Category

data BlogPostCategorySynopsis = BlogPostCategorySynopsis
  { blogPostCategorySynopsisName      :: BlogPostCategory
  , blogPostCategorySynopsisPermalink :: Permalink
  , blogPostCategorySynopsisPriority  :: BlogPostPriority
  } deriving (Eq, Show, Generic)

instance Arbitrary BlogPostCategorySynopsis where
  arbitrary = BlogPostCategorySynopsis <$> arbitrary
                                        <*> arbitrary
                                        <*> arbitrary

instance ToJSON BlogPostCategorySynopsis where
  toJSON BlogPostCategorySynopsis{..} = object
    [ "name" .= blogPostCategorySynopsisName
    , "permalink" .= blogPostCategorySynopsisPermalink
    , "priority" .= blogPostCategorySynopsisPriority
    ]

instance FromJSON BlogPostCategorySynopsis where
  parseJSON json = case json of
    Object o -> BlogPostCategorySynopsis <$> o .: "name"
                                          <*> o .: "permalink"
                                          <*> o .: "priority"
    _ -> typeMismatch "BlogPostCategorySynopsis" json


data GetBlogPostCategory = GetBlogPostCategory
  { getBlogPostCategoryName      :: BlogPostCategory
  , getBlogPostCategoryPermalink :: Permalink
  , getBlogPostCategoryPrimary   :: Maybe BlogPostSynopsis
  , getBlogPostCategoryPosts     :: [BlogPostSynopsis]
  , getBlogPostCategoryId        :: StoredBlogPostCategoryId
  } deriving (Eq, Show, Generic)

instance Arbitrary GetBlogPostCategory where
  arbitrary = GetBlogPostCategory <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary

instance ToJSON GetBlogPostCategory where
  toJSON GetBlogPostCategory{..} = object
    [ "name" .= getBlogPostCategoryName
    , "permalink" .= getBlogPostCategoryPermalink
    , "primary" .= getBlogPostCategoryPrimary
    , "posts" .= getBlogPostCategoryPosts
    , "id" .= getBlogPostCategoryId
    ]

instance FromJSON GetBlogPostCategory where
  parseJSON json = case json of
    Object o -> GetBlogPostCategory <$> o .: "name"
                                    <*> o .: "permalink"
                                    <*> o .: "primary"
                                    <*> o .: "posts"
                                    <*> o .: "id"
    _ -> typeMismatch "GetBlogPostCategory" json


data NewBlogPostCategory = NewBlogPostCategory
  { newBlogPostCategoryName      :: BlogPostCategory
  , newBlogPostCategoryPermalink :: Permalink
  , newBlogPostCategoryPriority  :: BlogPostPriority
  } deriving (Eq, Show, Generic)

instance Arbitrary NewBlogPostCategory where
  arbitrary = NewBlogPostCategory <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary

instance ToJSON NewBlogPostCategory where
  toJSON NewBlogPostCategory{..} = object
    [ "name" .= newBlogPostCategoryName
    , "permalink" .= newBlogPostCategoryPermalink
    , "priority" .= newBlogPostCategoryPriority
    ]

instance FromJSON NewBlogPostCategory where
  parseJSON json = case json of
    Object o -> NewBlogPostCategory <$> o .: "name"
                                    <*> o .: "permalink"
                                    <*> o .: "priority"
    _ -> typeMismatch "NewBlogPostCategory" json


data SetBlogPostCategory = SetBlogPostCategory
  { setBlogPostCategoryName      :: BlogPostCategory
  , setBlogPostCategoryPermalink :: Permalink
  , setBlogPostCategoryPriority  :: BlogPostPriority
  , setBlogPostCategoryId        :: StoredBlogPostCategoryId
  } deriving (Eq, Show, Generic)

instance Arbitrary SetBlogPostCategory where
  arbitrary = SetBlogPostCategory <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary

instance ToJSON SetBlogPostCategory where
  toJSON SetBlogPostCategory{..} = object
    [ "name" .= setBlogPostCategoryName
    , "permalink" .= setBlogPostCategoryPermalink
    , "priority" .= setBlogPostCategoryPriority
    , "id" .= setBlogPostCategoryId
    ]

instance FromJSON SetBlogPostCategory where
  parseJSON json = case json of
    Object o -> SetBlogPostCategory <$> o .: "name"
                                    <*> o .: "permalink"
                                    <*> o .: "priority"
                                    <*> o .: "id"
    _ -> typeMismatch "SetBlogPostCategory" json


-- * Post

data BlogPostSynopsis = BlogPostSynopsis
  { blogPostSynopsisAuthor    :: Name
  , blogPostSynopsisTimestamp :: UTCTime
  , blogPostSynopsisHeadline  :: Text
  , blogPostSynopsisPermalink :: Permalink
  , blogPostSynopsisVariant   :: BlogPostVariant
  , blogPostSynopsisPriority  :: BlogPostPriority
  } deriving (Eq, Show, Generic)


instance Arbitrary BlogPostSynopsis where
  arbitrary = BlogPostSynopsis <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

instance ToJSON BlogPostSynopsis where
  toJSON BlogPostSynopsis{..} = object
    [ "author" .= blogPostSynopsisAuthor
    , "timestamp" .= blogPostSynopsisTimestamp
    , "headline" .= blogPostSynopsisHeadline
    , "permalink" .= blogPostSynopsisPermalink
    , "variant" .= blogPostSynopsisVariant
    , "priority" .= blogPostSynopsisPriority
    ]

instance FromJSON BlogPostSynopsis where
  parseJSON json = case json of
    Object o -> BlogPostSynopsis <$> o .: "author"
                                  <*> o .: "timestamp"
                                  <*> o .: "headline"
                                  <*> o .: "permalink"
                                  <*> o .: "variant"
                                  <*> o .: "priority"
    _ -> typeMismatch "BlogPostSynopsis" json


data GetBlogPost = GetBlogPost
  { getBlogPostAuthor    :: Name
  , getBlogPostTimestamp :: UTCTime
  , getBlogPostHeadline  :: Text
  , getBlogPostPermalink :: Permalink
  , getBlogPostContent   :: MarkdownText
  , getBlogPostVariant   :: BlogPostVariant
  , getBlogPostPriority  :: BlogPostPriority
  , getBlogPostCategory  :: BlogPostCategory
  , getBlogPostId        :: StoredBlogPostId
  } deriving (Eq, Show, Generic)


instance Arbitrary GetBlogPost where
  arbitrary = GetBlogPost <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
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
    , "variant" .= getBlogPostVariant
    , "priority" .= getBlogPostPriority
    , "category" .= getBlogPostCategory
    , "id" .= getBlogPostId
    ]

instance FromJSON GetBlogPost where
  parseJSON json = case json of
    Object o -> GetBlogPost <$> o .: "author"
                            <*> o .: "timestamp"
                            <*> o .: "headline"
                            <*> o .: "permalink"
                            <*> o .: "content"
                            <*> o .: "variant"
                            <*> o .: "priority"
                            <*> o .: "category"
                            <*> o .: "id"
    _ -> typeMismatch "GetBlogPost" json


data NewBlogPost = NewBlogPost
  { newBlogPostHeadline  :: Text
  , newBlogPostPermalink :: Permalink
  , newBlogPostContent   :: MarkdownText
  , newBlogPostVariant   :: BlogPostVariant
  , newBlogPostPriority  :: BlogPostPriority
  , newBlogPostCategory  :: StoredBlogPostCategoryId
  , newBlogPostPrimary   :: Bool
  } deriving (Eq, Show, Generic)


instance Arbitrary NewBlogPost where
  arbitrary = NewBlogPost <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance ToJSON NewBlogPost where
  toJSON NewBlogPost{..} = object
    [ "headline" .= newBlogPostHeadline
    , "permalink" .= newBlogPostPermalink
    , "content" .= newBlogPostContent
    , "variant" .= newBlogPostVariant
    , "priority" .= newBlogPostPriority
    , "category" .= newBlogPostCategory
    , "primary" .= newBlogPostPrimary
    ]

instance FromJSON NewBlogPost where
  parseJSON json = case json of
    Object o -> NewBlogPost <$> o .: "headline"
                            <*> o .: "permalink"
                            <*> o .: "content"
                            <*> o .: "variant"
                            <*> o .: "priority"
                            <*> o .: "category"
                            <*> o .: "primary"
    _ -> typeMismatch "NewBlogPost" json


data SetBlogPost = SetBlogPost
  { setBlogPostHeadline  :: Text
  , setBlogPostPermalink :: Permalink
  , setBlogPostContent   :: MarkdownText
  , setBlogPostVariant   :: BlogPostVariant
  , setBlogPostPriority  :: BlogPostPriority
  , setBlogPostPrimary   :: Bool
  , setBlogPostId        :: StoredBlogPostId
  } deriving (Eq, Show, Generic)


instance Arbitrary SetBlogPost where
  arbitrary = SetBlogPost <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance ToJSON SetBlogPost where
  toJSON SetBlogPost{..} = object
    [ "headline" .= setBlogPostHeadline
    , "permalink" .= setBlogPostPermalink
    , "content" .= setBlogPostContent
    , "variant" .= setBlogPostVariant
    , "priority" .= setBlogPostPriority
    , "primary" .= setBlogPostPrimary
    , "id" .= setBlogPostId
    ]

instance FromJSON SetBlogPost where
  parseJSON json = case json of
    Object o -> SetBlogPost <$> o .: "headline"
                            <*> o .: "permalink"
                            <*> o .: "content"
                            <*> o .: "variant"
                            <*> o .: "priority"
                            <*> o .: "primary"
                            <*> o .: "id"
    _ -> typeMismatch "SetBlogPost" json

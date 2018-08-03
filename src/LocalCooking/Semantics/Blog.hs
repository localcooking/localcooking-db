{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , DeriveFunctor
  #-}

module LocalCooking.Semantics.Blog where

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
  , getBlogPostCategoryVariant   :: BlogPostVariant
  , getBlogPostCategoryId        :: StoredBlogPostCategoryId
  } deriving (Eq, Show, Generic)

instance Arbitrary GetBlogPostCategory where
  arbitrary = GetBlogPostCategory <$> arbitrary
                                  <*> arbitrary
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
    , "variant" .= getBlogPostCategoryVariant
    , "id" .= getBlogPostCategoryId
    ]

instance FromJSON GetBlogPostCategory where
  parseJSON json = case json of
    Object o -> GetBlogPostCategory <$> o .: "name"
                                    <*> o .: "permalink"
                                    <*> o .: "primary"
                                    <*> o .: "posts"
                                    <*> o .: "variant"
                                    <*> o .: "id"
    _ -> typeMismatch "GetBlogPostCategory" json



data NewBlogPostCategory = NewBlogPostCategory
  { newBlogPostCategoryName      :: BlogPostCategory
  , newBlogPostCategoryPermalink :: Permalink
  , newBlogPostCategoryPriority  :: BlogPostPriority
  , newBlogPostCategoryVariant   :: BlogPostVariant
  } deriving (Eq, Show, Generic)

instance Arbitrary NewBlogPostCategory where
  arbitrary = NewBlogPostCategory <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary

instance ToJSON NewBlogPostCategory where
  toJSON NewBlogPostCategory{..} = object
    [ "name" .= newBlogPostCategoryName
    , "permalink" .= newBlogPostCategoryPermalink
    , "priority" .= newBlogPostCategoryPriority
    , "variant" .= newBlogPostCategoryVariant
    ]

instance FromJSON NewBlogPostCategory where
  parseJSON json = case json of
    Object o -> NewBlogPostCategory <$> o .: "name"
                                    <*> o .: "permalink"
                                    <*> o .: "priority"
                                    <*> o .: "variant"
    _ -> typeMismatch "NewBlogPostCategory" json


data SetBlogPostCategory = SetBlogPostCategory
  { setBlogPostCategoryName      :: BlogPostCategory
  , setBlogPostCategoryPermalink :: Permalink
  , setBlogPostCategoryPriority  :: BlogPostPriority
  , setBlogPostCategoryVariant   :: BlogPostVariant
  , setBlogPostCategoryId        :: StoredBlogPostCategoryId
  } deriving (Eq, Show, Generic)

instance Arbitrary SetBlogPostCategory where
  arbitrary = SetBlogPostCategory <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary

instance ToJSON SetBlogPostCategory where
  toJSON SetBlogPostCategory{..} = object
    [ "name" .= setBlogPostCategoryName
    , "permalink" .= setBlogPostCategoryPermalink
    , "priority" .= setBlogPostCategoryPriority
    , "variant" .= setBlogPostCategoryVariant
    , "id" .= setBlogPostCategoryId
    ]

instance FromJSON SetBlogPostCategory where
  parseJSON json = case json of
    Object o -> SetBlogPostCategory <$> o .: "name"
                                    <*> o .: "permalink"
                                    <*> o .: "priority"
                                    <*> o .: "variant"
                                    <*> o .: "id"
    _ -> typeMismatch "SetBlogPostCategory" json


-- * Post

data BlogPostSynopsis = BlogPostSynopsis
  { blogPostSynopsisAuthor    :: Name
  , blogPostSynopsisTimestamp :: UTCTime
  , blogPostSynopsisHeadline  :: Text
  , blogPostSynopsisPermalink :: Permalink
  , blogPostSynopsisPriority  :: BlogPostPriority
  } deriving (Eq, Show, Generic)


instance Arbitrary BlogPostSynopsis where
  arbitrary = BlogPostSynopsis <$> arbitrary
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
    , "priority" .= blogPostSynopsisPriority
    ]

instance FromJSON BlogPostSynopsis where
  parseJSON json = case json of
    Object o -> BlogPostSynopsis <$> o .: "author"
                                  <*> o .: "timestamp"
                                  <*> o .: "headline"
                                  <*> o .: "permalink"
                                  <*> o .: "priority"
    _ -> typeMismatch "BlogPostSynopsis" json


data GetBlogPost = GetBlogPost
  { getBlogPostAuthor    :: Name
  , getBlogPostTimestamp :: UTCTime
  , getBlogPostHeadline  :: Text
  , getBlogPostPermalink :: Permalink
  , getBlogPostContent   :: MarkdownText
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

instance ToJSON GetBlogPost where
  toJSON GetBlogPost{..} = object
    [ "author" .= getBlogPostAuthor
    , "timestamp" .= getBlogPostTimestamp
    , "headline" .= getBlogPostHeadline
    , "permalink" .= getBlogPostPermalink
    , "content" .= getBlogPostContent
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
                            <*> o .: "priority"
                            <*> o .: "category"
                            <*> o .: "id"
    _ -> typeMismatch "GetBlogPost" json


data NewBlogPost = NewBlogPost
  { newBlogPostHeadline  :: Text
  , newBlogPostPermalink :: Permalink
  , newBlogPostContent   :: MarkdownText
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

instance ToJSON NewBlogPost where
  toJSON NewBlogPost{..} = object
    [ "headline" .= newBlogPostHeadline
    , "permalink" .= newBlogPostPermalink
    , "content" .= newBlogPostContent
    , "priority" .= newBlogPostPriority
    , "category" .= newBlogPostCategory
    , "primary" .= newBlogPostPrimary
    ]

instance FromJSON NewBlogPost where
  parseJSON json = case json of
    Object o -> NewBlogPost <$> o .: "headline"
                            <*> o .: "permalink"
                            <*> o .: "content"
                            <*> o .: "priority"
                            <*> o .: "category"
                            <*> o .: "primary"
    _ -> typeMismatch "NewBlogPost" json


data SetBlogPost = SetBlogPost
  { setBlogPostHeadline  :: Text
  , setBlogPostPermalink :: Permalink
  , setBlogPostContent   :: MarkdownText
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

instance ToJSON SetBlogPost where
  toJSON SetBlogPost{..} = object
    [ "headline" .= setBlogPostHeadline
    , "permalink" .= setBlogPostPermalink
    , "content" .= setBlogPostContent
    , "priority" .= setBlogPostPriority
    , "primary" .= setBlogPostPrimary
    , "id" .= setBlogPostId
    ]

instance FromJSON SetBlogPost where
  parseJSON json = case json of
    Object o -> SetBlogPost <$> o .: "headline"
                            <*> o .: "permalink"
                            <*> o .: "content"
                            <*> o .: "priority"
                            <*> o .: "primary"
                            <*> o .: "id"
    _ -> typeMismatch "SetBlogPost" json



-- * Errors

data BlogPostCategoryExists a
  = BlogPostCategoryDoesntExist
  | BlogPostCategoryExists a
  deriving (Eq, Show, Generic, Functor)

instance Applicative BlogPostCategoryExists where
  pure = BlogPostCategoryExists
  (<*>) f x = case f of
    BlogPostCategoryDoesntExist -> BlogPostCategoryDoesntExist
    BlogPostCategoryExists f' -> f' <$> x

instance Monad BlogPostCategoryExists where
  return = pure
  (>>=) x f = case x of
    BlogPostCategoryDoesntExist -> BlogPostCategoryDoesntExist
    BlogPostCategoryExists x' -> f x'

instance Arbitrary a => Arbitrary (BlogPostCategoryExists a) where
  arbitrary = oneof
    [ pure BlogPostCategoryDoesntExist
    , BlogPostCategoryExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (BlogPostCategoryExists a) where
  toJSON x = case x of
    BlogPostCategoryDoesntExist -> String "blogPostCategoryDoesntExist"
    BlogPostCategoryExists a -> object ["blogPostCategoryExists" .= a]

instance FromJSON a => FromJSON (BlogPostCategoryExists a) where
  parseJSON x = case x of
    String s
      | s == "blogPostCategoryDoesntExist" -> pure BlogPostCategoryDoesntExist
      | otherwise -> fail'
    Object o -> BlogPostCategoryExists <$> o .: "blogPostCategoryExists"
    _ -> fail'
    where
      fail' = typeMismatch "BlogPostCategoryExists" x

data BlogPostCategoryUnique a
  = BlogPostCategoryNotUnique
  | BlogPostCategoryUnique a
  deriving (Eq, Show, Generic, Functor)

instance Applicative BlogPostCategoryUnique where
  pure = BlogPostCategoryUnique
  (<*>) f x = case f of
    BlogPostCategoryNotUnique -> BlogPostCategoryNotUnique
    BlogPostCategoryUnique f' -> f' <$> x

instance Monad BlogPostCategoryUnique where
  return = pure
  (>>=) x f = case x of
    BlogPostCategoryNotUnique -> BlogPostCategoryNotUnique
    BlogPostCategoryUnique x' -> f x'

instance Arbitrary a => Arbitrary (BlogPostCategoryUnique a) where
  arbitrary = oneof
    [ pure BlogPostCategoryNotUnique
    , BlogPostCategoryUnique <$> arbitrary
    ]

instance ToJSON a => ToJSON (BlogPostCategoryUnique a) where
  toJSON x = case x of
    BlogPostCategoryNotUnique -> String "blogPostCategoryNotUnique"
    BlogPostCategoryUnique a -> object ["blogPostCategoryUnique" .= a]

instance FromJSON a => FromJSON (BlogPostCategoryUnique a) where
  parseJSON x = case x of
    String s
      | s == "blogPostCategoryNotUnique" -> pure BlogPostCategoryNotUnique
      | otherwise -> fail'
    Object o -> BlogPostCategoryUnique <$> o .: "blogPostCategoryUnique"
    _ -> fail'
    where
      fail' = typeMismatch "BlogPostCategoryUnique" x


data BlogPostExists a
  = BlogPostDoesntExist
  | BlogPostExists a
  deriving (Eq, Show, Generic, Functor)

instance Applicative BlogPostExists where
  pure = BlogPostExists
  (<*>) f x = case f of
    BlogPostDoesntExist -> BlogPostDoesntExist
    BlogPostExists f' -> f' <$> x

instance Monad BlogPostExists where
  return = pure
  (>>=) x f = case x of
    BlogPostDoesntExist -> BlogPostDoesntExist
    BlogPostExists x' -> f x'

instance Arbitrary a => Arbitrary (BlogPostExists a) where
  arbitrary = oneof
    [ pure BlogPostDoesntExist
    , BlogPostExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (BlogPostExists a) where
  toJSON x = case x of
    BlogPostDoesntExist -> String "blogPostDoesntExist"
    BlogPostExists a -> object ["blogPostExists" .= a]

instance FromJSON a => FromJSON (BlogPostExists a) where
  parseJSON x = case x of
    String s
      | s == "blogPostDoesntExist" -> pure BlogPostDoesntExist
      | otherwise -> fail'
    Object o -> BlogPostExists <$> o .: "blogPostExists"
    _ -> fail'
    where
      fail' = typeMismatch "BlogPostExists" x

data BlogPostUnique a
  = BlogPostNotUnique
  | BlogPostUnique a
  deriving (Eq, Show, Generic, Functor)

instance Applicative BlogPostUnique where
  pure = BlogPostUnique
  (<*>) f x = case f of
    BlogPostNotUnique -> BlogPostNotUnique
    BlogPostUnique f' -> f' <$> x

instance Monad BlogPostUnique where
  return = pure
  (>>=) x f = case x of
    BlogPostNotUnique -> BlogPostNotUnique
    BlogPostUnique x' -> f x'

instance Arbitrary a => Arbitrary (BlogPostUnique a) where
  arbitrary = oneof
    [ pure BlogPostNotUnique
    , BlogPostUnique <$> arbitrary
    ]

instance ToJSON a => ToJSON (BlogPostUnique a) where
  toJSON x = case x of
    BlogPostNotUnique -> String "blogPostNotUnique"
    BlogPostUnique a -> object ["blogPostUnique" .= a]

instance FromJSON a => FromJSON (BlogPostUnique a) where
  parseJSON x = case x of
    String s
      | s == "blogPostNotUnique" -> pure BlogPostNotUnique
      | otherwise -> fail'
    Object o -> BlogPostUnique <$> o .: "blogPostUnique"
    _ -> fail'
    where
      fail' = typeMismatch "BlogPostUnique" x


data BlogPostPrimary a
  = BlogPostNotPrimary
  | BlogPostPrimary a
  deriving (Eq, Show, Generic, Functor)

instance Applicative BlogPostPrimary where
  pure = BlogPostPrimary
  (<*>) f x = case f of
    BlogPostNotPrimary -> BlogPostNotPrimary
    BlogPostPrimary f' -> f' <$> x

instance Monad BlogPostPrimary where
  return = pure
  (>>=) x f = case x of
    BlogPostNotPrimary -> BlogPostNotPrimary
    BlogPostPrimary x' -> f x'

instance Arbitrary a => Arbitrary (BlogPostPrimary a) where
  arbitrary = oneof
    [ pure BlogPostNotPrimary
    , BlogPostPrimary <$> arbitrary
    ]

instance ToJSON a => ToJSON (BlogPostPrimary a) where
  toJSON x = case x of
    BlogPostNotPrimary -> String "blogPostNotPrimary"
    BlogPostPrimary a -> object ["blogPostPrimary" .= a]

instance FromJSON a => FromJSON (BlogPostPrimary a) where
  parseJSON x = case x of
    String s
      | s == "blogPostNotPrimary" -> pure BlogPostNotPrimary
      | otherwise -> fail'
    Object o -> BlogPostPrimary <$> o .: "blogPostPrimary"
    _ -> fail'
    where
      fail' = typeMismatch "BlogPostPrimary" x

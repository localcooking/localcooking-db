{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , DeriveFunctor
  #-}

module LocalCooking.Semantics.Chef where

import LocalCooking.Database.Schema (StoredMealId, StoredOrderId)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Order (OrderProgress)

import Data.Name (Name)
import Data.Price (Price)
import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()


-- * Profile

data SetChef = SetChef
  { setChefName      :: Maybe Name
  , setChefPermalink :: Maybe Permalink
  , setChefImages    :: [ImageSource]
  , setChefAvatar    :: Maybe ImageSource
  , setChefBio       :: MarkdownText
  , setChefTags      :: [ChefTag]
  } deriving (Eq, Show, Generic)

emptySetChef :: SetChef
emptySetChef = SetChef Nothing Nothing [] Nothing "" []

instance Arbitrary SetChef where
  arbitrary = SetChef <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance ToJSON SetChef where
  toJSON SetChef{..} = object
    [ "name" .= setChefName
    , "permalink" .= setChefPermalink
    , "images" .= setChefImages
    , "avatar" .= setChefAvatar
    , "bio" .= setChefBio
    , "tags" .= setChefTags
    ]

instance FromJSON SetChef where
  parseJSON json = case json of
    Object o -> SetChef <$> o .: "name"
                        <*> o .: "permalink"
                        <*> o .: "images"
                        <*> o .: "avatar"
                        <*> o .: "bio"
                        <*> o .: "tags"
    _ -> typeMismatch "SetChef" json


data ChefValid = ChefValid
  { chefValidName      :: Name
  , chefValidPermalink :: Permalink
  , chefValidImages    :: [ImageSource]
  , chefValidAvatar    :: ImageSource
  , chefValidBio       :: MarkdownText
  , chefValidTags      :: [ChefTag]
  } deriving (Eq, Show, Generic)

instance Arbitrary ChefValid where
  arbitrary = ChefValid <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance ToJSON ChefValid where
  toJSON ChefValid{..} = object
    [ "name" .= chefValidName
    , "permalink" .= chefValidPermalink
    , "images" .= chefValidImages
    , "avatar" .= chefValidAvatar
    , "bio" .= chefValidBio
    , "tags" .= chefValidTags
    ]

instance FromJSON ChefValid where
  parseJSON json = case json of
    Object o -> ChefValid <$> o .: "name"
                          <*> o .: "permalink"
                          <*> o .: "images"
                          <*> o .: "avatar"
                          <*> o .: "bio"
                          <*> o .: "tags"
    _ -> typeMismatch "ChefValid" json


-- * Subject Matter

-- submission with a StoredMealId constitutes an update
data MealSettings = MealSettings
  { mealSettingsTitle        :: Text
  , mealSettingsPermalink    :: Permalink
  , mealSettingsHeading      :: Text
  , mealSettingsDescription  :: MarkdownText
  , mealSettingsInstructions :: MarkdownText
  , mealSettingsImages       :: [ImageSource]
  , mealSettingsIngredients  :: [IngredientTag]
  , mealSettingsTags         :: [MealTag]
  , mealSettingsPrice        :: Price
  } deriving (Eq, Show, Generic)

instance Arbitrary MealSettings where
  arbitrary = MealSettings <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON MealSettings where
  toJSON MealSettings{..} = object
    [ "title" .= mealSettingsTitle
    , "permalink" .= mealSettingsPermalink
    , "heading" .= mealSettingsHeading
    , "description" .= mealSettingsDescription
    , "instructions" .= mealSettingsInstructions
    , "images" .= mealSettingsImages
    , "ingredients" .= mealSettingsIngredients
    , "tags" .= mealSettingsTags
    , "price" .= mealSettingsPrice
    ]

instance FromJSON MealSettings where
  parseJSON json = case json of
    Object o -> MealSettings <$> o .: "title"
                             <*> o .: "permalink"
                             <*> o .: "heading"
                             <*> o .: "description"
                             <*> o .: "instructions"
                             <*> o .: "images"
                             <*> o .: "ingredients"
                             <*> o .: "tags"
                             <*> o .: "price"
    _ -> typeMismatch "MealSettings" json


-- submission with a StoredMenuId constitutes an update
data MenuSettings = MenuSettings
  { menuSettingsPublished   :: Maybe Day -- ^ Special treatment when Just
  , menuSettingsDeadline    :: Day
  , menuSettingsHeading     :: Text
  , menuSettingsDescription :: MarkdownText
  , menuSettingsTags        :: [MealTag] -- ^ featured from meals
  , menuSettingsImages      :: [ImageSource] -- ^ featured from meals
  } deriving (Eq, Show, Generic)

instance Arbitrary MenuSettings where
  arbitrary = MenuSettings <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON MenuSettings where
  toJSON MenuSettings{..} = object
    [ "published" .= menuSettingsPublished
    , "deadline" .= menuSettingsDeadline
    , "heading" .= menuSettingsHeading
    , "description" .= menuSettingsDescription
    , "tags" .= menuSettingsTags
    , "images" .= menuSettingsImages
    ]

instance FromJSON MenuSettings where
  parseJSON json = case json of
    Object o -> MenuSettings <$> o .: "published"
                             <*> o .: "deadline"
                             <*> o .: "heading"
                             <*> o .: "description"
                             <*> o .: "tags"
                             <*> o .: "images"
    _ -> typeMismatch "MenuSettings" json


data Order = Order
  { orderMeal     :: StoredMealId
  , orderProgress :: OrderProgress
  , orderVolume   :: Int
  , orderId       :: StoredOrderId
  , orderTime     :: UTCTime
  } deriving (Eq, Show, Generic)

instance Arbitrary Order where
  arbitrary = Order <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance ToJSON Order where
  toJSON Order{..} = object
    [ "meal" .= orderMeal
    , "progress" .= orderProgress
    , "volume" .= orderVolume
    , "id" .= orderId
    , "time" .= orderTime
    ]

instance FromJSON Order where
  parseJSON json = case json of
    Object o -> Order <$> o .: "meal"
                      <*> o .: "progress"
                      <*> o .: "volume"
                      <*> o .: "id"
                      <*> o .: "time"
    _ -> typeMismatch "Order" json


-- * Errors

data ChefExists a
  = ChefDoesntExist
  | ChefExists a
  deriving (Eq, Show, Generic, Functor)

instance Applicative ChefExists where
  pure = ChefExists
  (<*>) f x = case f of
    ChefDoesntExist -> ChefDoesntExist
    ChefExists f' -> f' <$> x

instance Monad ChefExists where
  return = pure
  (>>=) x f = case x of
    ChefDoesntExist -> ChefDoesntExist
    ChefExists x' -> f x'

instance Arbitrary a => Arbitrary (ChefExists a) where
  arbitrary = oneof
    [ pure ChefDoesntExist
    , ChefExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (ChefExists a) where
  toJSON x = case x of
    ChefDoesntExist -> String "chefDoesntExist"
    ChefExists a -> object ["chefExists" .= a]

instance FromJSON a => FromJSON (ChefExists a) where
  parseJSON x = case x of
    String s
      | s == "chefDoesntExist" -> pure ChefDoesntExist
      | otherwise -> fail'
    Object o -> ChefExists <$> o .: "chefExists"
    _ -> fail'
    where
      fail' = typeMismatch "ChefExists" x



data ChefUnique a
  = ChefNotUnique
  | ChefUnique a
  deriving (Eq, Show, Generic, Functor)

instance Applicative ChefUnique where
  pure = ChefUnique
  (<*>) f x = case f of
    ChefNotUnique -> ChefNotUnique
    ChefUnique f' -> f' <$> x

instance Monad ChefUnique where
  return = pure
  (>>=) x f = case x of
    ChefNotUnique -> ChefNotUnique
    ChefUnique x' -> f x'

instance Arbitrary a => Arbitrary (ChefUnique a) where
  arbitrary = oneof
    [ pure ChefNotUnique
    , ChefUnique <$> arbitrary
    ]

instance ToJSON a => ToJSON (ChefUnique a) where
  toJSON x = case x of
    ChefNotUnique -> String "chefNotUnique"
    ChefUnique a -> object ["chefUnique" .= a]

instance FromJSON a => FromJSON (ChefUnique a) where
  parseJSON x = case x of
    String s
      | s == "chefNotUnique" -> pure ChefNotUnique
      | otherwise -> fail'
    Object o -> ChefUnique <$> o .: "chefUnique"
    _ -> fail'
    where
      fail' = typeMismatch "ChefUnique" x

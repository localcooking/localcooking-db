{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantics.Chef where

import LocalCooking.Database.Schema.Semantics (StoredMealId, StoredOrderId)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Ingredient (IngredientName)
import LocalCooking.Common.Order (OrderProgress)

import Data.Price (Price)
import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Time.Calendar (Day)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


-- unique key is tied to the user who submits it -- StoredUserId is a valid id
data ChefSettings = ChefSettings
  { chefSettingsName      :: Name
  , chefSettingsPermalink :: Permalink
  , chefSettingsImages    :: [ImageSource]
  , chefSettingsAvatar    :: ImageSource
  , chefSettingsBio       :: MarkdownText
  , chefSettingsTags      :: [ChefTag]
  } deriving (Eq, Show, Generic)


instance Arbitrary ChefSettings where
  arbitrary = ChefSettings <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON ChefSettings where
  toJSON ChefSettings{..} = object
    [ "name" .= chefSettingsName
    , "permalink" .= chefSettingsPermalink
    , "images" .= chefSettingsImages
    , "avatar" .= chefSettingsAvatar
    , "bio" .= chefSettingsBio
    , "tags" .= chefSettingsTags
    ]

instance FromJSON ChefSettings where
  parseJSON json = case json of
    Object o -> ChefSettings <$> o .: "name"
                             <*> o .: "permalink"
                             <*> o .: "images"
                             <*> o .: "avatar"
                             <*> o .: "bio"
                             <*> o .: "tags"
    _ -> typeMismatch "ChefSettings" json


-- submission with a StoredMealId constitutes an update
data MealSettings = MealSettings
  { mealSettingsTitle        :: Text
  , mealSettingsPermalink    :: Permalink
  , mealSettingsHeading      :: Text
  , mealSettingsDescription  :: MarkdownText
  , mealSettingsInstructions :: MarkdownText
  , mealSettingsImages       :: [ImageSource]
  , mealSettingsIngredients  :: [IngredientName]
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


-- Only updates are submitted - update order progress.
data Order = Order
  { orderMeal     :: StoredMealId
  , orderProgress :: OrderProgress
  , orderVolume   :: Int
  , orderId       :: StoredOrderId
  } deriving (Eq, Show, Generic)

instance Arbitrary Order where
  arbitrary = Order <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance ToJSON Order where
  toJSON Order{..} = object
    [ "meal" .= orderMeal
    , "progress" .= orderProgress
    , "volume" .= orderVolume
    , "id" .= orderId
    ]

instance FromJSON Order where
  parseJSON json = case json of
    Object o -> Order <$> o .: "meal"
                      <*> o .: "progress"
                      <*> o .: "volume"
                      <*> o .: "id"
    _ -> typeMismatch "Order" json

{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , NamedFieldPuns
  #-}

module LocalCooking.Semantics.Mitch where

import LocalCooking.Database.Schema.Semantics (StoredReviewId)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Ingredient (Ingredient, IngredientName)
import LocalCooking.Common.Order (OrderProgress)

import Data.Address (USAAddress)
import Data.Time (UTCTime)
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


-- * Reviews

-- | Note that this is from the customers' public perspectives - in the Chef interface,
-- reviews will have additional user and order identifiers for Chef customer satisfaction
-- management
data ReviewSynopsis = ReviewSynopsis
  { reviewSynopsisRating  :: Rating
  , reviewSynopsisHeading :: Text
  , reviewSynopsisId      :: StoredReviewId -- ^ Backlink for clicking
  } deriving (Eq, Show, Generic)

instance Arbitrary ReviewSynopsis where
  arbitrary = ReviewSynopsis <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary

instance ToJSON ReviewSynopsis where
  toJSON ReviewSynopsis{..} = object
    [ "rating" .= reviewSynopsisRating
    , "heading" .= reviewSynopsisHeading
    , "id" .= reviewSynopsisId
    ]

instance FromJSON ReviewSynopsis where
  parseJSON json = case json of
    Object o -> ReviewSynopsis <$> o .: "rating"
                               <*> o .: "heading"
                               <*> o .: "id"
    _ -> typeMismatch "ReviewSynopsis" json


getReviewSynopsis :: Review -> ReviewSynopsis
getReviewSynopsis Review{reviewRating,reviewHeading,reviewId} = ReviewSynopsis
  { reviewSynopsisRating = reviewRating
  , reviewSynopsisId = reviewId
  , reviewSynopsisHeading = reviewHeading
  }


data Review = Review
  { reviewRating    :: Rating
  , reviewSubmitted :: UTCTime
  , reviewHeading   :: Text
  , reviewId        :: StoredReviewId -- ^ Backlink for HREF
  , reviewBody      :: MarkdownText
  , reviewImages    :: [ImageSource] -- ^ Evidence images uploaded by customer
  } deriving (Eq, Show, Generic)

instance Arbitrary Review where
  arbitrary = Review <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary


instance ToJSON Review where
  toJSON Review{..} = object
    [ "rating" .= reviewRating
    , "submitted" .= reviewSubmitted
    , "heading" .= reviewHeading
    , "id" .= reviewId
    , "body" .= reviewBody
    , "images" .= reviewImages
    ]

instance FromJSON Review where
  parseJSON json = case json of
    Object o -> Review <$> o .: "rating"
                       <*> o .: "submitted"
                       <*> o .: "heading"
                       <*> o .: "id"
                       <*> o .: "body"
                       <*> o .: "images"
    _ -> typeMismatch "Review" json



-- * Menus

-- | Perspective from Customers - doesn't including editing information to Chefs
data MenuSynopsis = MenuSynopsis
  { menuSynopsisPublished :: Day
  , menuSynopsisDeadline  :: Day
  , menuSynopsisHeading   :: Text
  , menuSynopsisTags      :: [MealTag] -- ^ Featured tags from multiset of meals' tags
  , menuSynopsisImages    :: [ImageSource] -- ^ Featured images from multiset of meals' images
  } deriving (Eq, Show, Generic)


instance Arbitrary MenuSynopsis where
  arbitrary = MenuSynopsis <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON MenuSynopsis where
  toJSON MenuSynopsis{..} = object
    [ "published" .= menuSynopsisPublished
    , "deadline" .= menuSynopsisDeadline
    , "heading" .= menuSynopsisHeading
    , "tags" .= menuSynopsisTags
    , "images" .= menuSynopsisImages
    ]

instance FromJSON MenuSynopsis where
  parseJSON json = case json of
    Object o -> MenuSynopsis <$> o .: "published"
                             <*> o .: "deadline"
                             <*> o .: "heading"
                             <*> o .: "tags"
                             <*> o .: "images"
    _ -> typeMismatch "MenuSynopsis" json


data Menu = Menu
  { menuPublished   :: Day
  , menuDeadline    :: Day
  , menuDescription :: MarkdownText
  , menuAuthor      :: ChefSynopsis -- ^ Corner seller reference, like Amazon or Ebay
  , menuMeals       :: [MealSynopsis] -- ^ Featured items
  } deriving (Eq, Show, Generic)


instance Arbitrary Menu where
  arbitrary = Menu <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON Menu where
  toJSON Menu{..} = object
    [ "published" .= menuPublished
    , "deadline" .= menuDeadline
    , "description" .= menuDescription
    , "author" .= menuAuthor
    , "meals" .= menuMeals
    ]

instance FromJSON Menu where
  parseJSON json = case json of
    Object o -> Menu <$> o .: "published"
                     <*> o .: "deadline"
                     <*> o .: "description"
                     <*> o .: "author"
                     <*> o .: "meals"
    _ -> typeMismatch "Menu" json


-- * Meals

data MealSynopsis = MealSynopsis
  { mealSynopsisTitle     :: Text
  , mealSynopsisPermalink :: Permalink -- ^ Backlink for clicking
  , mealSynopsisHeading   :: Text
  , mealSynopsisImages    :: [ImageSource] -- ^ Featured images of meal, for listing
  , mealSynopsisRating    :: Rating
  , mealSynopsisOrders    :: Int -- ^ Like "number of purchases" - for customer confidence
  , mealSynopsisTags      :: [MealTag]
  , mealSynopsisDiets     :: [Diet]
  , mealSynopsisPrice     :: Price
  } deriving (Eq, Show, Generic)


instance Arbitrary MealSynopsis where
  arbitrary = MealSynopsis <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary


instance ToJSON MealSynopsis where
  toJSON MealSynopsis{..} = object
    [ "title" .= mealSynopsisTitle
    , "permalink" .= mealSynopsisPermalink
    , "heading" .= mealSynopsisHeading
    , "images" .= mealSynopsisImages
    , "rating" .= mealSynopsisRating
    , "orders" .= mealSynopsisOrders
    , "tags" .= mealSynopsisTags
    , "diets" .= mealSynopsisDiets
    , "price" .= mealSynopsisPrice
    ]

instance FromJSON MealSynopsis where
  parseJSON json = case json of
    Object o -> MealSynopsis <$> o .: "title"
                             <*> o .: "permalink"
                             <*> o .: "heading"
                             <*> o .: "images"
                             <*> o .: "rating"
                             <*> o .: "orders"
                             <*> o .: "tags"
                             <*> o .: "diets"
                             <*> o .: "price"
    _ -> typeMismatch "MealSynopsis" json


data Meal = Meal
  { mealTitle        :: Text
  , mealPermalink    :: Permalink -- ^ Backlink for HREF
  , mealDescription  :: MarkdownText
  , mealInstructions :: MarkdownText
  , mealImages       :: [ImageSource]
  , mealIngredients  :: [Ingredient]
  , mealDiets        :: [Diet] -- ^ derived from Ingredients listing in DB
  , mealTags         :: [MealTag]
  , mealOrders       :: Int
  , mealRating       :: Rating
  , mealReviews      :: [ReviewSynopsis]
  , mealPrice        :: Price
  } deriving (Eq, Show, Generic)


instance Arbitrary Meal where
  arbitrary = Meal <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON Meal where
  toJSON Meal{..} = object
    [ "title" .= mealTitle
    , "permalink" .= mealPermalink
    , "description" .= mealDescription
    , "instructions" .= mealInstructions
    , "images" .= mealImages
    , "ingredients" .= mealIngredients
    , "diets" .= mealDiets
    , "tags" .= mealTags
    , "orders" .= mealOrders
    , "rating" .= mealRating
    , "reviews" .= mealReviews
    , "price" .= mealPrice
    ]

instance FromJSON Meal where
  parseJSON json = case json of
    Object o -> Meal <$> o .: "title"
                     <*> o .: "permalink"
                     <*> o .: "description"
                     <*> o .: "instructions"
                     <*> o .: "images"
                     <*> o .: "ingredients"
                     <*> o .: "diets"
                     <*> o .: "tags"
                     <*> o .: "orders"
                     <*> o .: "rating"
                     <*> o .: "reviews"
                     <*> o .: "price"
    _ -> typeMismatch "Meal" json


-- * Chefs

data ChefSynopsis = ChefSynopsis
  { chefSynopsisName      :: Name
  , chefSynopsisPermalink :: Permalink -- ^ Backlink for clicking
  , chefSynopsisImage     :: ImageSource -- ^ Primary Chef profile image
  , chefSynopsisRating    :: Rating
  , chefSynopsisOrders    :: Int
  , chefSynopsisTags      :: [ChefTag]
  } deriving (Eq, Show, Generic)


instance Arbitrary ChefSynopsis where
  arbitrary = ChefSynopsis <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON ChefSynopsis where
  toJSON ChefSynopsis{..} = object
    [ "name" .= chefSynopsisName
    , "permalink" .= chefSynopsisPermalink
    , "image" .= chefSynopsisImage
    , "rating" .= chefSynopsisRating
    , "orders" .= chefSynopsisOrders
    , "tags" .= chefSynopsisTags
    ]

instance FromJSON ChefSynopsis where
  parseJSON json = case json of
    Object o -> ChefSynopsis <$> o .: "name"
                             <*> o .: "permalink"
                             <*> o .: "image"
                             <*> o .: "rating"
                             <*> o .: "orders"
                             <*> o .: "tags"
    _ -> typeMismatch "ChefSynopsis" json


data Chef = Chef
  { chefName         :: Name
  , chefPermalink    :: Permalink -- ^ Backlink for HREF
  , chefImages       :: [ImageSource] -- ^ All chef images
  , chefBio          :: MarkdownText
  , chefRating       :: Rating
  , chefReviews      :: [ReviewSynopsis]
  , chefActiveOrders :: Int
  , chefTotalOrders  :: Int
  , chefTags         :: [ChefTag]
  , chefMenus        :: [MenuSynopsis] -- ^ Active menus -- TODO historic menus, too?
  } deriving (Eq, Show, Generic)


instance Arbitrary Chef where
  arbitrary = Chef <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON Chef where
  toJSON Chef{..} = object
    [ "name" .= chefName
    , "permalink" .= chefPermalink
    , "images" .= chefImages
    , "bio" .= chefBio
    , "rating" .= chefRating
    , "reviews" .= chefReviews
    , "activeOrders" .= chefActiveOrders
    , "totalOrders" .= chefTotalOrders
    , "tags" .= chefTags
    , "menus" .= chefMenus
    ]

instance FromJSON Chef where
  parseJSON json = case json of
    Object o -> Chef <$> o .: "name"
                     <*> o .: "permalink"
                     <*> o .: "images"
                     <*> o .: "bio"
                     <*> o .: "rating"
                     <*> o .: "reviews"
                     <*> o .: "activeOrders"
                     <*> o .: "totalOrders"
                     <*> o .: "tags"
                     <*> o .: "menus"
    _ -> typeMismatch "Chef" json



data Order = Order
  { orderMeal     :: MealSynopsis
  , orderProgress :: OrderProgress
  , orderTime     :: UTCTime
  } deriving (Eq, Show, Generic)

instance Arbitrary Order where
  arbitrary = Order <$> arbitrary <*> arbitrary <*> arbitrary

instance ToJSON Order where
  toJSON Order{..} = object
    [ "meal" .= orderMeal
    , "progress" .= orderProgress
    , "time" .= orderTime
    ]

instance FromJSON Order where
  parseJSON json = case json of
    Object o -> Order <$> o .: "meal" <*> o .: "progress" <*> o .: "time"
    _ -> typeMismatch "Order" json



data Customer = Customer
  { customerName      :: Name
  , customerAddress   :: USAAddress
  , customerDiets     :: [Diet]
  , customerAllergies :: [IngredientName]
  } deriving (Eq, Show, Generic)

instance Arbitrary Customer where
  arbitrary = Customer <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

instance ToJSON Customer where
  toJSON Customer{..} = object
    [ "name" .= customerName
    , "address" .= customerAddress
    , "diets" .= customerDiets
    , "allergies" .= customerAllergies
    ]

instance FromJSON Customer where
  parseJSON json = case json of
    Object o -> Customer <$> o .: "name"
                         <*> o .: "address"
                         <*> o .: "diets"
                         <*> o .: "allergies"
    _ -> typeMismatch "Customer" json

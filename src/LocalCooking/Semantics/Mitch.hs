{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , DeriveFunctor
  , NamedFieldPuns
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Semantics.Mitch where

import LocalCooking.Database.Schema (StoredReviewId, StoredMealId, StoredOrderId)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Order (OrderProgress)

import Data.Name (Name)
import Data.Address (USAAddress)
import Data.Time (UTCTime)
import Data.Price (Price)
import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Time.Calendar (Day)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()



-- * Profile

data SetCustomer = SetCustomer
  { setCustomerName    :: Maybe Name
  , setCustomerAddress :: Maybe USAAddress
  } deriving (Eq, Show, Generic)

emptySetCustomer :: SetCustomer
emptySetCustomer = SetCustomer Nothing Nothing

instance Arbitrary SetCustomer where
  arbitrary = SetCustomer  <$> arbitrary
                           <*> arbitrary

instance ToJSON SetCustomer where
  toJSON SetCustomer{..} = object
    [ "name" .= setCustomerName
    , "address" .= setCustomerAddress
    ]

instance FromJSON SetCustomer where
  parseJSON json = case json of
    Object o -> SetCustomer  <$> o .: "name"
                             <*> o .: "address"
    _ -> typeMismatch "SetCustomer" json

data CustomerValid = CustomerValid
  { customerValidName    :: Name
  , customerValidAddress :: USAAddress
  } deriving (Eq, Show, Generic)

instance Arbitrary CustomerValid where
  arbitrary = CustomerValid  <$> arbitrary
                           <*> arbitrary

instance ToJSON CustomerValid where
  toJSON CustomerValid{..} = object
    [ "name" .= customerValidName
    , "address" .= customerValidAddress
    ]

instance FromJSON CustomerValid where
  parseJSON json = case json of
    Object o -> CustomerValid  <$> o .: "name"
                             <*> o .: "address"
    _ -> typeMismatch "CustomerValid" json


-- ** Other Customer Components

newtype Diets = Diets
  { getDiets :: [DietTag]
  } deriving (Eq, Show, Generic, Arbitrary, ToJSON, FromJSON)

newtype Allergies = Allergies
  { getAllergies :: [IngredientTag]
  } deriving (Eq, Show, Generic, Arbitrary, ToJSON, FromJSON)


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

data SubmitReview = SubmitReview
  { submitReviewOrder   :: StoredOrderId
  , submitReviewRating  :: Rating
  , submitReviewHeading :: Text
  , submitReviewBody    :: MarkdownText
  , submitReviewImages  :: [ImageSource]
  } deriving (Eq, Show, Generic)

instance ToJSON SubmitReview where
  toJSON SubmitReview{..} = object
    [ "order" .= submitReviewOrder
    , "rating" .= submitReviewRating
    , "heading" .= submitReviewHeading
    , "body" .= submitReviewBody
    , "images" .= submitReviewImages
    ]

instance FromJSON SubmitReview where
  parseJSON json = case json of
    Object o -> SubmitReview
            <$> o .: "order"
            <*> o .: "rating"
            <*> o .: "heading"
            <*> o .: "body"
            <*> o .: "images"
    _ -> typeMismatch "SubmitReview" json




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
  , mealSynopsisDiets     :: [DietTag]
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
  , mealDiets        :: [DietTag] -- ^ derived from Ingredients listing in DB
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
  , chefMenus        :: [MenuSynopsis] -- ^ Active menus -- TODO historic menus, too? Search component*
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




-- * Cart Mechanics

data Order = Order
  { orderMeal     :: MealSynopsis
  , orderProgress :: OrderProgress
  , orderTime     :: UTCTime
  , orderVolume   :: Int
  } deriving (Eq, Show, Generic)

instance Arbitrary Order where
  arbitrary = Order <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToJSON Order where
  toJSON Order{..} = object
    [ "meal" .= orderMeal
    , "progress" .= orderProgress
    , "time" .= orderTime
    , "volume" .= orderVolume
    ]

instance FromJSON Order where
  parseJSON json = case json of
    Object o -> Order <$> o .: "meal"
                      <*> o .: "progress"
                      <*> o .: "time"
                      <*> o .: "volume"
    _ -> typeMismatch "Order" json




data CartEntry = CartEntry
  { cartEntryMeal   :: StoredMealId
  , cartEntryVolume :: Int
  , cartEntryAdded  :: UTCTime
  } deriving (Eq, Show, Generic)

instance Arbitrary CartEntry where
  arbitrary = CartEntry <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance ToJSON CartEntry where
  toJSON CartEntry{..} = object
    [ "meal" .= cartEntryMeal
    , "volume" .= cartEntryVolume
    , "added" .= cartEntryAdded
    ]

instance FromJSON CartEntry where
  parseJSON json = case json of
    Object o -> CartEntry <$> o .: "meal"
                          <*> o .: "volume"
                          <*> o .: "added"
    _ -> typeMismatch "CartEntry" json


-- * Browse

data BrowseMenu = BrowseMenu
  { browseMenuChef :: Permalink
  , browseMenuDeadline :: Day
  } deriving (Eq, Show, Generic)

uncurryBrowseMenu :: (Permalink -> Day -> a) -> BrowseMenu -> a
uncurryBrowseMenu f (BrowseMenu a b) = f a b

instance ToJSON BrowseMenu where
  toJSON BrowseMenu{..} = object
    [ "chef" .= browseMenuChef
    , "deadline" .= browseMenuDeadline
    ]

instance FromJSON BrowseMenu where
  parseJSON json = case json of
    Object o -> BrowseMenu <$> o .: "chef" <*> o .: "deadline"
    _ -> typeMismatch "BrowseMenu" json


data BrowseMeal = BrowseMeal
  { browseMealChef     :: Permalink
  , browseMealDeadline :: Day
  , browseMealMeal     :: Permalink
  } deriving (Eq, Show, Generic)

uncurryBrowseMeal :: (Permalink -> Day -> Permalink -> a) -> BrowseMeal -> a
uncurryBrowseMeal f (BrowseMeal a b c) = f a b c

instance ToJSON BrowseMeal where
  toJSON BrowseMeal{..} = object
    [ "chef" .= browseMealChef
    , "deadline" .= browseMealDeadline
    , "meal" .= browseMealMeal
    ]

instance FromJSON BrowseMeal where
  parseJSON json = case json of
    Object o -> BrowseMeal <$> o .: "chef" <*> o .: "deadline" <*> o .: "meal"
    _ -> typeMismatch "BrowseMeal" json


data AddToCart = AddToCart
  { addToCartChef     :: Permalink
  , addToCartDeadline :: Day
  , addToCartMeal     :: Permalink
  , addToCartVolume   :: Int
  } deriving (Eq, Show, Generic)

uncurryAddToCart :: (Permalink -> Day -> Permalink -> Int -> a) -> AddToCart -> a
uncurryAddToCart f (AddToCart a b c d) = f a b c d

instance ToJSON AddToCart where
  toJSON AddToCart{..} = object
    [ "chef" .= addToCartChef
    , "deadline" .= addToCartDeadline
    , "meal" .= addToCartMeal
    , "volume" .= addToCartVolume
    ]

instance FromJSON AddToCart where
  parseJSON json = case json of
    Object o -> AddToCart <$> o .: "chef"
                          <*> o .: "deadline"
                          <*> o .: "meal"
                          <*> o .: "volume"
    _ -> typeMismatch "AddToCart" json






-- * Errors

data CustomerExists a
  = CustomerDoesntExist
  | CustomerExists a
  deriving (Eq, Show, Generic, Functor)

instance Applicative CustomerExists where
  pure = CustomerExists
  (<*>) f x = case f of
    CustomerDoesntExist -> CustomerDoesntExist
    CustomerExists f' -> f' <$> x

instance Monad CustomerExists where
  return = pure
  (>>=) x f = case x of
    CustomerDoesntExist -> CustomerDoesntExist
    CustomerExists x' -> f x'

instance Arbitrary a => Arbitrary (CustomerExists a) where
  arbitrary = oneof
    [ pure CustomerDoesntExist
    , CustomerExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (CustomerExists a) where
  toJSON x = case x of
    CustomerDoesntExist -> String "customerDoesntExist"
    CustomerExists a -> object ["customerExists" .= a]

instance FromJSON a => FromJSON (CustomerExists a) where
  parseJSON x = case x of
    String s
      | s == "customerDoesntExist" -> pure CustomerDoesntExist
      | otherwise -> fail'
    Object o -> CustomerExists <$> o .: "customerExists"
    _ -> fail'
    where
      fail' = typeMismatch "CustomerExists" x


data CustomerUnique a
  = CustomerNotUnique
  | CustomerUnique a
  deriving (Eq, Show, Generic, Functor)

instance Applicative CustomerUnique where
  pure = CustomerUnique
  (<*>) f x = case f of
    CustomerNotUnique -> CustomerNotUnique
    CustomerUnique f' -> f' <$> x

instance Monad CustomerUnique where
  return = pure
  (>>=) x f = case x of
    CustomerNotUnique -> CustomerNotUnique
    CustomerUnique x' -> f x'

instance Arbitrary a => Arbitrary (CustomerUnique a) where
  arbitrary = oneof
    [ pure CustomerNotUnique
    , CustomerUnique <$> arbitrary
    ]

instance ToJSON a => ToJSON (CustomerUnique a) where
  toJSON x = case x of
    CustomerNotUnique -> String "customerNotUnique"
    CustomerUnique a -> object ["customerUnique" .= a]

instance FromJSON a => FromJSON (CustomerUnique a) where
  parseJSON x = case x of
    String s
      | s == "customerNotUnique" -> pure CustomerNotUnique
      | otherwise -> fail'
    Object o -> CustomerUnique <$> o .: "customerUnique"
    _ -> fail'
    where
      fail' = typeMismatch "CustomerUnique" x


data OrderExists a
  = OrderDoesntExist
  | OrderExists a
  deriving (Eq, Show, Generic, Functor)

instance Applicative OrderExists where
  pure = OrderExists
  (<*>) f x = case f of
    OrderDoesntExist -> OrderDoesntExist
    OrderExists f' -> f' <$> x

instance Monad OrderExists where
  return = pure
  (>>=) x f = case x of
    OrderDoesntExist -> OrderDoesntExist
    OrderExists x' -> f x'

instance Arbitrary a => Arbitrary (OrderExists a) where
  arbitrary = oneof
    [ pure OrderDoesntExist
    , OrderExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (OrderExists a) where
  toJSON x = case x of
    OrderDoesntExist -> String "orderDoesntExist"
    OrderExists a -> object ["orderExists" .= a]

instance FromJSON a => FromJSON (OrderExists a) where
  parseJSON x = case x of
    String s
      | s == "orderDoesntExist" -> pure OrderDoesntExist
      | otherwise -> fail'
    Object o -> OrderExists <$> o .: "orderExists"
    _ -> fail'
    where
      fail' = typeMismatch "OrderExists" x


data ReviewExists a
  = ReviewDoesntExist
  | ReviewExists a
  deriving (Eq, Show, Generic, Functor)

reviewExistsToMaybe :: ReviewExists a -> Maybe a
reviewExistsToMaybe x = case x of
  ReviewDoesntExist -> Nothing
  ReviewExists y -> Just y

instance Applicative ReviewExists where
  pure = ReviewExists
  (<*>) f x = case f of
    ReviewDoesntExist -> ReviewDoesntExist
    ReviewExists f' -> f' <$> x

instance Monad ReviewExists where
  return = pure
  (>>=) x f = case x of
    ReviewDoesntExist -> ReviewDoesntExist
    ReviewExists x' -> f x'

instance Arbitrary a => Arbitrary (ReviewExists a) where
  arbitrary = oneof
    [ pure ReviewDoesntExist
    , ReviewExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (ReviewExists a) where
  toJSON x = case x of
    ReviewDoesntExist -> String "reviewDoesntExist"
    ReviewExists a -> object ["reviewExists" .= a]

instance FromJSON a => FromJSON (ReviewExists a) where
  parseJSON x = case x of
    String s
      | s == "reviewDoesntExist" -> pure ReviewDoesntExist
      | otherwise -> fail'
    Object o -> ReviewExists <$> o .: "reviewExists"
    _ -> fail'
    where
      fail' = typeMismatch "ReviewExists" x

data RatingExists a
  = RatingDoesntExist
  | RatingExists a
  deriving (Eq, Show, Generic, Functor)

ratingExistsToMaybe :: RatingExists a -> Maybe a
ratingExistsToMaybe x = case x of
  RatingDoesntExist -> Nothing
  RatingExists y -> Just y

instance Applicative RatingExists where
  pure = RatingExists
  (<*>) f x = case f of
    RatingDoesntExist -> RatingDoesntExist
    RatingExists f' -> f' <$> x

instance Monad RatingExists where
  return = pure
  (>>=) x f = case x of
    RatingDoesntExist -> RatingDoesntExist
    RatingExists x' -> f x'

instance Arbitrary a => Arbitrary (RatingExists a) where
  arbitrary = oneof
    [ pure RatingDoesntExist
    , RatingExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (RatingExists a) where
  toJSON x = case x of
    RatingDoesntExist -> String "ratingDoesntExist"
    RatingExists a -> object ["ratingExists" .= a]

instance FromJSON a => FromJSON (RatingExists a) where
  parseJSON x = case x of
    String s
      | s == "ratingDoesntExist" -> pure RatingDoesntExist
      | otherwise -> fail'
    Object o -> RatingExists <$> o .: "ratingExists"
    _ -> fail'
    where
      fail' = typeMismatch "RatingExists" x


data MealExists a
  = MealDoesntExist
  | MealExists a
  deriving (Eq, Show, Generic, Functor)

mealExistsToMaybe :: MealExists a -> Maybe a
mealExistsToMaybe x = case x of
  MealDoesntExist -> Nothing
  MealExists y -> Just y

instance Applicative MealExists where
  pure = MealExists
  (<*>) f x = case f of
    MealDoesntExist -> MealDoesntExist
    MealExists f' -> f' <$> x

instance Monad MealExists where
  return = pure
  (>>=) x f = case x of
    MealDoesntExist -> MealDoesntExist
    MealExists x' -> f x'

instance Arbitrary a => Arbitrary (MealExists a) where
  arbitrary = oneof
    [ pure MealDoesntExist
    , MealExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (MealExists a) where
  toJSON x = case x of
    MealDoesntExist -> String "mealDoesntExist"
    MealExists a -> object ["mealExists" .= a]

instance FromJSON a => FromJSON (MealExists a) where
  parseJSON x = case x of
    String s
      | s == "mealDoesntExist" -> pure MealDoesntExist
      | otherwise -> fail'
    Object o -> MealExists <$> o .: "mealExists"
    _ -> fail'
    where
      fail' = typeMismatch "MealExists" x


data MenuExists a
  = MenuDoesntExist
  | MenuExists a
  deriving (Eq, Show, Generic, Functor)

menuExistsToMaybe :: MenuExists a -> Maybe a
menuExistsToMaybe x = case x of
  MenuDoesntExist -> Nothing
  MenuExists y -> Just y

instance Applicative MenuExists where
  pure = MenuExists
  (<*>) f x = case f of
    MenuDoesntExist -> MenuDoesntExist
    MenuExists f' -> f' <$> x

instance Monad MenuExists where
  return = pure
  (>>=) x f = case x of
    MenuDoesntExist -> MenuDoesntExist
    MenuExists x' -> f x'

instance Arbitrary a => Arbitrary (MenuExists a) where
  arbitrary = oneof
    [ pure MenuDoesntExist
    , MenuExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (MenuExists a) where
  toJSON x = case x of
    MenuDoesntExist -> String "menuDoesntExist"
    MenuExists a -> object ["menuExists" .= a]

instance FromJSON a => FromJSON (MenuExists a) where
  parseJSON x = case x of
    String s
      | s == "menuDoesntExist" -> pure MenuDoesntExist
      | otherwise -> fail'
    Object o -> MenuExists <$> o .: "menuExists"
    _ -> fail'
    where
      fail' = typeMismatch "MenuExists" x




data MenuUnique a
  = MenuNotUnique
  | MenuUnique a
  deriving (Eq, Show, Generic, Functor)

instance Applicative MenuUnique where
  pure = MenuUnique
  (<*>) f x = case f of
    MenuNotUnique -> MenuNotUnique
    MenuUnique f' -> f' <$> x

instance Monad MenuUnique where
  return = pure
  (>>=) x f = case x of
    MenuNotUnique -> MenuNotUnique
    MenuUnique x' -> f x'

instance Arbitrary a => Arbitrary (MenuUnique a) where
  arbitrary = oneof
    [ pure MenuNotUnique
    , MenuUnique <$> arbitrary
    ]

instance ToJSON a => ToJSON (MenuUnique a) where
  toJSON x = case x of
    MenuNotUnique -> String "menuNotUnique"
    MenuUnique a -> object ["menuUnique" .= a]

instance FromJSON a => FromJSON (MenuUnique a) where
  parseJSON x = case x of
    String s
      | s == "menuNotUnique" -> pure MenuNotUnique
      | otherwise -> fail'
    Object o -> MenuUnique <$> o .: "menuUnique"
    _ -> fail'
    where
      fail' = typeMismatch "MenuUnique" x



data MealUnique a
  = MealNotUnique
  | MealUnique a
  deriving (Eq, Show, Generic, Functor)

instance Applicative MealUnique where
  pure = MealUnique
  (<*>) f x = case f of
    MealNotUnique -> MealNotUnique
    MealUnique f' -> f' <$> x

instance Monad MealUnique where
  return = pure
  (>>=) x f = case x of
    MealNotUnique -> MealNotUnique
    MealUnique x' -> f x'

instance Arbitrary a => Arbitrary (MealUnique a) where
  arbitrary = oneof
    [ pure MealNotUnique
    , MealUnique <$> arbitrary
    ]

instance ToJSON a => ToJSON (MealUnique a) where
  toJSON x = case x of
    MealNotUnique -> String "mealNotUnique"
    MealUnique a -> object ["mealUnique" .= a]

instance FromJSON a => FromJSON (MealUnique a) where
  parseJSON x = case x of
    String s
      | s == "mealNotUnique" -> pure MealNotUnique
      | otherwise -> fail'
    Object o -> MealUnique <$> o .: "mealUnique"
    _ -> fail'
    where
      fail' = typeMismatch "MealUnique" x



data MenuPublished a
  = MenuNotPublished
  | MenuPublished a
  deriving (Eq, Show, Generic, Functor)

instance Applicative MenuPublished where
  pure = MenuPublished
  (<*>) f x = case f of
    MenuNotPublished -> MenuNotPublished
    MenuPublished f' -> f' <$> x

instance Monad MenuPublished where
  return = pure
  (>>=) x f = case x of
    MenuNotPublished -> MenuNotPublished
    MenuPublished x' -> f x'

instance Arbitrary a => Arbitrary (MenuPublished a) where
  arbitrary = oneof
    [ pure MenuNotPublished
    , MenuPublished <$> arbitrary
    ]

instance ToJSON a => ToJSON (MenuPublished a) where
  toJSON x = case x of
    MenuNotPublished -> String "menuNotPublished"
    MenuPublished a -> object ["menuPublished" .= a]

instance FromJSON a => FromJSON (MenuPublished a) where
  parseJSON x = case x of
    String s
      | s == "menuNotPublished" -> pure MenuNotPublished
      | otherwise -> fail'
    Object o -> MenuPublished <$> o .: "menuPublished"
    _ -> fail'
    where
      fail' = typeMismatch "MenuPublished" x

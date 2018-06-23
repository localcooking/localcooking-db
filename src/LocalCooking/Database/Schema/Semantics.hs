{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , PartialTypeSignatures
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Semantics where

import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Schema.User.Customer (StoredCustomerId)
import LocalCooking.Database.Schema.Tag.Meal (StoredMealTagId)
import LocalCooking.Database.Schema.Tag.Chef (StoredChefTagId)
import LocalCooking.Database.Schema.Tag.Ingredient (StoredIngredientTagId)
import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.User.Name (Name)

import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Markdown (MarkdownText)
import Data.Text.Permalink (Permalink)
import Data.Price (Price)
import Data.Hashable (Hashable (..))
import Data.Aeson (encode)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Database.Persist.Sql (toSqlKey)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Test.QuickCheck (Arbitrary (..))
import Unsafe.Coerce (unsafeCoerce)



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredMeal
    storedMealTitle Text
    storedMealPermalink Permalink
    storedMealMenu StoredMenuId
    storedMealHeading Text
    storedMealDescription MarkdownText
    storedMealInstructions MarkdownText
    storedMealImages [ImageSource]
    storedMealPrice Price
    UniqueMealPermalink storedMealMenu storedMealPermalink
    deriving Eq Show

MealIngredient
    mealIngredientMeal StoredMealId
    mealIngredientIngredient StoredIngredientTagId
    UniqueMealIngredient mealIngredientMeal mealIngredientIngredient
    deriving Eq Show

MealTagRelation
    mealTagMeal StoredMealId
    mealTagMealTag StoredMealTagId
    UniqueMealTag mealTagMeal mealTagMealTag
    deriving Eq Show

StoredMenu
    storedMenuPublished Day Maybe
    storedMenuDeadline Day
    storedMenuHeading Text
    storedMenuDescription MarkdownText
    storedMenuImages [ImageSource]
    storedMenuAuthor StoredChefId
    UniqueMenuDeadline storedMenuAuthor storedMenuDeadline
    deriving Eq Show

MenuTagRelation
    menuTagMenu StoredMenuId
    menuTagMealTag StoredMealTagId
    UniqueMenuTag menuTagMenu menuTagMealTag
    deriving Eq Show

StoredChef
    storedChefOwner StoredUserId
    storedChefName Name
    storedChefPermalink Permalink
    storedChefBio MarkdownText
    storedChefImages [ImageSource]
    storedChefAvatar ImageSource
    UniqueChefOwner storedChefOwner
    UniqueChefPermalink storedChefPermalink
    deriving Eq Show

ChefTagRelation
    chefTagChef StoredChefId
    chefTagChefTag StoredChefTagId
    UniqueChefTag chefTagChef chefTagChefTag
    deriving Eq Show

StoredReview
    storedReviewOrder StoredOrderId
    storedReviewChef StoredChefId
    storedReviewMeal StoredMealId
    storedReviewAuthor StoredCustomerId
    storedReviewRating Rating
    storedReviewSubmitted UTCTime
    storedReviewHeading Text
    storedReviewBody MarkdownText
    storedReviewImages [ImageSource]
    UniqueReviewAuthor storedReviewAuthor storedReviewOrder
    deriving Eq Show

StoredOrder
    storedOrderCustomer StoredCustomerId
    storedOrderMeal StoredMealId
    storedOrderMenu StoredMenuId
    storedOrderChef StoredChefId
    storedOrderVolume Int
    storedOrderProgress OrderProgress
    storedOrderTime UTCTime
    deriving Eq Show


CartRelation
    cartRelationCustomer StoredUserId
    cartRelationMeal StoredMealId
    cartRelationVolume Int
    cartRelationAdded UTCTime
    UniqueCartRelation cartRelationCustomer cartRelationMeal
    deriving Eq Show
|]


instance Arbitrary StoredReviewId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredReviewId where
  hashWithSalt s x = hashWithSalt s (encode x)


instance Arbitrary StoredOrderId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredOrderId where
  hashWithSalt s x = hashWithSalt s (encode x)


instance Arbitrary StoredMealId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredMealId where
  hashWithSalt s x = hashWithSalt s (encode x)


instance Arbitrary StoredChefId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredChefId where
  hashWithSalt s x = hashWithSalt s (encode x)


instance Arbitrary StoredMenuId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredMenuId where
  hashWithSalt s x = hashWithSalt s (encode x)

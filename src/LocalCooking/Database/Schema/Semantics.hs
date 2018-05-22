{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Semantics where

import LocalCooking.Database.Schema.User (UserId)
import LocalCooking.Database.Schema.Tag.Meal (StoredMealTagId)
import LocalCooking.Common.Order (OrderProgress)

import Data.Image.Source (ImageSource)
import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


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
    UniqueMealPermalink storedMealPermalink
    deriving Eq Show

MealIngredient
    mealIngredientMeal StoredMealId
    mealIngredientIngredient StoredIngredientId
    UniqueMealsIngredient mealIngredientMeal mealIngredientIngredient
    deriving Eq Show

MealTag
    mealTagMeal StoredMealId
    mealTagMealTag StoredMealTagId
    UniqueMealsTag mealTagMeal mealTagMealTag
    deriving Eq Show

StoredMenu
    storedMenuPublished Day
    storedMenuDeadline Day
    storedMenuHeading Text
    storedMenuDescription MarkdownText
    storedMenuImages [ImageSource]
    storedMenuAuthor UserId
    deriving Eq Show

MenuTag
    menuTagMenu StoredMenuId
    menuTagMealTag StoredMealTagId
    UniqueMealsTag menuTagMenu menuTagMealTag
    deriving Eq Show

StoredChef
    storedChefOwner UserId
    storedChefName Name
    storedChefPermalink Permalink
    storedChefBio MarkdownText
    storedChefImages [ImageSource]
    storedChefAvatar ImageSource
    UniqueChefPermalink storedChefPermalink
    deriving Eq Show

ChefTag
    chefTagChef StoredChefId
    chefTagChefTag StoredChefTagId
    UniqueChefTag chefTagChef chefTagChefTag
    deriving Eq Show

NextReviewId
    nextReviewId ReviewId
    deriving Eq Show

StoredReview
    storedReviewOrder StoredOrderId
    storedReviewRating Rating
    storedReviewSubmitted UTCTime
    storedReviewHeading Text
    storedReviewId ReviewId
    storedReviewBody MarkdownText
    storedReviewImages [ImageSource]
    storedReviewAuthor UserId
    deriving Eq Show

StoredOrder
    storedOrderCustomer UserId
    storedOrderMeal StoredMealId
    storedOrderVolume Int
    storedOrderProgress OrderProgress
    storedOrderTime UTCTime
|]

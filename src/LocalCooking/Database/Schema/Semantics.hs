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

import LocalCooking.Database.Schema.Tag.Meal (StoredMealTagId)

import Data.Image.Source (ImageSource)
import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
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
    storedMealRating Rating
    storedMealOrders
    UniqueMealPermalink storedMealPermalink
    deriving Eq Show

MealsIngredient
    mealsIngredientMeal StoredMealId
    mealsIngredientIngredient StoredIngredientId
    UniqueMealsIngredient mealsIngredientMeal mealsIngredientIngredient
    deriving Eq Show

MealsTag
    mealsTagMeal StoredMealId
    mealsTagMealTag StoredMealTagId
    UniqueMealsTag mealsTagMeal mealsTagMealTag
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

NextReviewId
    nextReviewId ReviewId
    deriving Eq Show

StoredReview
    storedReviewRating Rating
    storedReviewSubmitted UTCTime
    storedReviewHeading Text
    storedReviewId ReviewId
    storedReviewBody MarkdownText
    storedReviewImages [ImageSource]
    deriving Eq Show
|]

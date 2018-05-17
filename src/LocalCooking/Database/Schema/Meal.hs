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

module LocalCooking.Database.Schema.Meal where

import LocalCooking.Database.Schema.Menu (StoredMenuId)
import LocalCooking.Database.Schema.Ingredient (StoredIngredientId)
import LocalCooking.Database.Schema.Tag.Meal (StoredMealTagId)

import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Markdown (MarkdownText)
import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredMeal
    storedMealTitle Text
    storedMealMenu StoredMenuId
    storedMealSynopsis Text
    storedMealDescription MarkdownText
    storedMealInstructions MarkdownText
    storedMealImages [ImageSource]
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
|]

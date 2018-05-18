{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , DeriveGeneric
  , RecordWildCards
  #-}

module LocalCooking.Database.Query.Meal where

import LocalCooking.Database.Query.Ingredient (getIngredientId, getIngredientById)
import LocalCooking.Database.Query.Tag.Meal (getMealTagId, getMealTagById)
import LocalCooking.Database.Schema.Menu (StoredMenuId)
import LocalCooking.Database.Schema.Meal (StoredMeal (..), MealsIngredient (..), MealsTag (..), EntityField (..))
import LocalCooking.Common.Meal (Meal (..))

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Text.EmailAddress (EmailAddress)
import Database.Persist (Entity (..), insert, insert_, delete, deleteBy, get, getBy, (=.), update, (==.), selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), oneof)



insertMeal :: ConnectionPool
           -> StoredMenuId
           -> Meal
           -> IO ()
insertMeal backend menuId Meal{..} =
  flip runSqlPool backend $ do
    mealId <- insert (StoredMeal mealTitle menuId mealSynopsis mealDescription mealInstructions mealImages)
    forM_ mealIngredients $ \i -> do
      mIngId <- liftIO (getIngredientId backend i)
      case mIngId of
        Nothing -> pure ()
        Just ingId -> insert_ (MealsIngredient mealId ingId)
    forM_ mealTags $ \t -> do
      mTagId <- liftIO (getMealTagId backend t)
      case mTagId of
        Nothing -> pure ()
        Just tagId -> insert_ (MealsTag mealId tagId)


getMeals :: ConnectionPool
         -> StoredMenuId
         -> IO [Meal]
getMeals backend menuId =
  flip runSqlPool backend $ do
    xs <- selectList [StoredMealStoredMealMenu ==. menuId] []
    forM xs $ \(Entity k (StoredMeal title _ synopsis desc inst images)) -> do
      ings <- selectList [MealsIngredientMealsIngredientMeal ==. k] []
      ingredients <- forM ings (\(Entity _ (MealsIngredient _ i)) -> liftIO (getIngredientById backend i))
      ts <- selectList [MealsTagMealsTagMeal ==. k] []
      tags <- forM ts (\(Entity _ (MealsTag _ i)) -> liftIO (getMealTagById backend i))
      pure Meal
        { mealTitle = title
        , mealSynopsis = synopsis
        , mealDescription = desc
        , mealInstructions = inst
        , mealImages = images
        , mealIngredients = catMaybes ingredients
        , mealTags = catMaybes tags
        }

{-# LANGUAGE
    RecordWildCards
  #-}

module LocalCooking.Database.Query.Semantics where

-- import LocalCooking.Database.Query.Ingredient (getIngredientId, getIngredientById)
import LocalCooking.Database.Query.Tag.Chef (getChefTagId)
import LocalCooking.Database.Query.Tag.Meal (getMealTagId)
import LocalCooking.Database.Query.IngredientDiet (getStoredIngredientId)
import LocalCooking.Database.Schema.Semantics
  ( StoredChef (..), StoredChefId, ChefTagRelation (..)
  , MealTagRelation (..), MealIngredient (..), StoredMeal (..), StoredMealId
  , MenuTagRelation (..), StoredMenu (..), StoredMenuId
  , EntityField
    ( MenuTagRelationMenuTagMenu, MenuTagRelationMenuTagMealTag
    , MealIngredientMealIngredientIngredient, MealIngredientMealIngredientMeal
    , MealTagRelationMealTagMeal, MealTagRelationMealTagMealTag
    )
  , Unique (UniqueChefPermalink, UniqueMealPermalink, UniqueMenuDeadline)
  )
import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Semantics.Chef (MealSettings (..), MenuSettings (..), ChefSettings (..))

import Data.Text.Permalink (Permalink)
import Data.Time.Calendar (Day)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Database.Persist
  ( Entity (..)
  , insert, insert_
  , deleteWhere
  , getBy
  , replace, (==.)
  , selectList
  )
import Database.Persist.Sql (ConnectionPool, runSqlPool)



getChefId :: ConnectionPool -> Permalink -> IO (Maybe StoredChefId)
getChefId backend permalink = do
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueChefPermalink permalink)
    case mEnt of
      Nothing -> pure Nothing
      Just (Entity k _) -> pure (Just k)



getMenuId :: ConnectionPool -> Permalink -> Day -> IO (Maybe StoredMenuId)
getMenuId backend permalink deadline =
  flip runSqlPool backend $ do
    mChef <- getBy (UniqueChefPermalink permalink)
    case mChef of
      Nothing -> pure Nothing
      Just (Entity chefId _) -> do
        mEnt <- getBy (UniqueMenuDeadline chefId deadline)
        case mEnt of
          Nothing -> pure Nothing
          Just (Entity k _) -> pure (Just k)



getMealId :: ConnectionPool -> Permalink -> Day -> Permalink -> IO (Maybe StoredMealId)
getMealId backend chefPermalink deadline mealPermalink =
  flip runSqlPool backend $ do
    mChef <- getBy (UniqueChefPermalink chefPermalink)
    case mChef of
      Nothing -> pure Nothing
      Just (Entity chefId _) -> do
        mMenu <- getBy (UniqueMenuDeadline chefId deadline)
        case mMenu of
          Nothing -> pure Nothing
          Just (Entity menuId _) -> do
            mEnt <- getBy (UniqueMealPermalink menuId mealPermalink)
            case mEnt of
              Nothing -> pure Nothing
              Just (Entity mealId _) -> pure (Just mealId)

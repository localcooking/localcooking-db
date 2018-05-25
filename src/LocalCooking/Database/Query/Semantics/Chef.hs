{-# LANGUAGE
    RecordWildCards
  #-}

module LocalCooking.Database.Query.Semantics.Chef where

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





setChef :: ConnectionPool
        -> StoredUserId
        -> ChefSettings
        -> IO Bool
setChef backend owner ChefSettings{..} =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueChefPermalink chefSettingsPermalink)
    case mEnt of
      Just _ -> pure False
      Nothing -> do
        chefId <- insert $ StoredChef
                    owner
                    chefSettingsName
                    chefSettingsPermalink
                    chefSettingsBio
                    chefSettingsImages
                    chefSettingsAvatar
        forM_ chefSettingsTags $ \tag -> do
          mId <- liftIO (getChefTagId backend tag)
          case mId of
            Nothing -> pure ()
            Just tagId -> insert_ (ChefTagRelation chefId tagId)
        pure True


setMenu :: ConnectionPool
        -> StoredChefId
        -> MenuSettings
        -> IO StoredMenuId
setMenu backend owner MenuSettings{..} =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueMenuDeadline owner menuSettingsDeadline)
    case mEnt of
      Nothing -> do
        menuId <- insert $ StoredMenu
          menuSettingsPublished
          menuSettingsDeadline
          menuSettingsHeading
          menuSettingsDescription
          menuSettingsImages
          owner
        forM_ menuSettingsTags $ \tag -> do
          mTagId <- liftIO (getMealTagId backend tag)
          case mTagId of
            Nothing -> pure ()
            Just tagId -> insert_ (MenuTagRelation menuId tagId)
        pure menuId
      Just (Entity menuId _) -> do
        replace menuId $ StoredMenu
          menuSettingsPublished
          menuSettingsDeadline
          menuSettingsHeading
          menuSettingsDescription
          menuSettingsImages
          owner
        newTags <- fmap catMaybes $ forM menuSettingsTags $ liftIO . getMealTagId backend
        oldTags <- fmap (fmap (\(Entity _ (MenuTagRelation _ tagId)) -> tagId))
                 $ selectList [MenuTagRelationMenuTagMenu ==. menuId] []
        let toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
            toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags
        forM_ toRemove $ \tagId ->
          deleteWhere
            [ MenuTagRelationMenuTagMenu ==. menuId
            , MenuTagRelationMenuTagMealTag ==. tagId
            ]
        forM_ toAdd $ \tagId ->
          insert_ (MenuTagRelation menuId tagId)
        pure menuId


setMeal :: ConnectionPool
        -> StoredMenuId
        -> MealSettings
        -> IO StoredMealId
setMeal backend menuId MealSettings{..} =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueMealPermalink menuId mealSettingsPermalink)
    case mEnt of
      Nothing -> do
        mealId <- insert $ StoredMeal
          mealSettingsTitle
          mealSettingsPermalink
          menuId
          mealSettingsHeading
          mealSettingsDescription
          mealSettingsInstructions
          mealSettingsImages
          mealSettingsPrice
        forM_ mealSettingsIngredients $ \ing -> do
          mIngId <- liftIO (getStoredIngredientId backend ing)
          case mIngId of
            Nothing -> pure ()
            Just ingId -> insert_ (MealIngredient mealId ingId)
        forM_ mealSettingsTags $ \tag -> do
          mTagId <- liftIO (getMealTagId backend tag)
          case mTagId of
            Nothing -> pure ()
            Just tagId -> insert_ (MealTagRelation mealId tagId)
        pure mealId
      Just (Entity mealId _) -> do
        replace mealId $ StoredMeal
          mealSettingsTitle
          mealSettingsPermalink
          menuId
          mealSettingsHeading
          mealSettingsDescription
          mealSettingsInstructions
          mealSettingsImages
          mealSettingsPrice
        newIngs <- fmap catMaybes $ forM mealSettingsIngredients $ liftIO . getStoredIngredientId backend
        oldIngs <- fmap (fmap (\(Entity _ (MealIngredient _ ingId)) -> ingId))
                 $ selectList [MealIngredientMealIngredientMeal ==. mealId] []
        let toRemove = Set.fromList oldIngs `Set.difference` Set.fromList newIngs
            toAdd = Set.fromList newIngs `Set.difference` Set.fromList oldIngs
        forM_ toRemove $ \ingId ->
          deleteWhere
            [ MealIngredientMealIngredientMeal ==. mealId
            , MealIngredientMealIngredientIngredient ==. ingId
            ]
        forM_ toAdd $ \ingId ->
          insert_ (MealIngredient mealId ingId)

        newTags <- fmap catMaybes $ forM mealSettingsTags $ liftIO . getMealTagId backend
        oldTags <- fmap (fmap (\(Entity _ (MealTagRelation _ tagId)) -> tagId))
                 $ selectList [MealTagRelationMealTagMeal ==. mealId] []
        let toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
            toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags
        forM_ toRemove $ \tagId ->
          deleteWhere
            [ MealTagRelationMealTagMeal ==. mealId
            , MealTagRelationMealTagMealTag ==. tagId
            ]
        forM_ toAdd $ \tagId ->
          insert_ (MealTagRelation mealId tagId)

        pure mealId

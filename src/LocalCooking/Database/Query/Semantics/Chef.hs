{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , DeriveGeneric
  , RecordWildCards
  #-}

module LocalCooking.Database.Query.Semantics.Chef where

-- import LocalCooking.Database.Query.Ingredient (getIngredientId, getIngredientById)
import LocalCooking.Database.Query.Tag.Chef (getChefTagId, getChefTagById)
import LocalCooking.Database.Query.Tag.Meal (getMealTagId, getMealTagById)
import LocalCooking.Database.Query.IngredientDiet (getStoredIngredientId, getIngredientById)
import LocalCooking.Database.Schema.Semantics
  ( StoredChef (..), ChefTag (..)
  , MealTag (..), MealIngredient (..), StoredMeal (..), StoredMealId
  , MenuTag (..), StoredMenu (..), StoredMenuId
  , EntityField
    ( MenuTagMenuTagMenu, MenuTagMenuTagMealTag
    , MealIngredientMealIngredientIngredient, MealIngredientMealIngredientMeal
    , MealTagMealTagMeal, MealTagMealTagMealTag
    )
  , Unique (UniqueChefPermalink, UniqueMealPermalink, UniqueMenuDeadline)
  )
import LocalCooking.Database.Schema.User (UserId)
import LocalCooking.Semantic.Chef (MealSettings (..), MenuSettings (..), ChefSettings (..))

import Data.Text.Permalink (Permalink)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Text.EmailAddress (EmailAddress)
import Database.Persist
  ( Entity (..)
  , insert, insert_
  , delete, deleteBy, deleteWhere
  , get, getBy
  , (=.), update, replace, (==.)
  , selectList
  )
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), oneof)


setChef :: ConnectionPool
        -> UserId
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
            Just tagId -> insert_ (ChefTag chefId tagId)
        pure True


setMenu :: ConnectionPool
        -> UserId
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
            Just tagId -> insert_ (MenuTag menuId tagId)
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
        oldTags <- fmap (fmap (\(Entity _ (MenuTag _ tagId)) -> tagId))
                 $ selectList [MenuTagMenuTagMenu ==. menuId] []
        let toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
            toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags
        forM_ toRemove $ \tagId ->
          deleteWhere [MenuTagMenuTagMenu ==. menuId, MenuTagMenuTagMealTag ==. tagId]
        forM_ toAdd $ \tagId -> do
          insert_ (MenuTag menuId tagId)
        pure menuId


setMeal :: ConnectionPool
        -> StoredMenuId
        -> MealSettings
        -> IO StoredMealId
setMeal backend menuId MealSettings{..} =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueMealPermalink mealSettingsPermalink)
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
            Just tagId -> insert_ (MealTag mealId tagId)
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
        forM_ toRemove $ \ingId -> do
          deleteWhere
            [ MealIngredientMealIngredientMeal ==. mealId
            , MealIngredientMealIngredientIngredient ==. ingId
            ]
        forM_ toAdd $ \ingId -> do
          insert_ (MealIngredient mealId ingId)

        newTags <- fmap catMaybes $ forM mealSettingsTags $ liftIO . getMealTagId backend
        oldTags <- fmap (fmap (\(Entity _ (MealTag _ tagId)) -> tagId))
                 $ selectList [MealTagMealTagMeal ==. mealId] []
        let toRemove = Set.fromList oldTags `Set.difference` Set.fromList newTags
            toAdd = Set.fromList newTags `Set.difference` Set.fromList oldTags
        forM_ toRemove $ \tagId -> do
          deleteWhere [MealTagMealTagMeal ==. mealId, MealTagMealTagMealTag ==. tagId]
        forM_ toAdd $ \tagId -> do
          insert_ (MealTag mealId tagId)

        pure mealId

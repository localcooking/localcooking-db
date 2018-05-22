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
import LocalCooking.Database.Schema.Semantics
  ( StoredChef (..), ChefTag (..)
  , MealTag (..)
  , MenuTag (..), StoredMenu (..), StoredMenuId
  , EntityField (MenuTagMenuTagMenu, MenuTagMenuTagMealTag)
  , Unique (UniqueChefPermalink, UniqueMenuDeadline)
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
            Just tagId ->
              insert_ (ChefTag chefId tagId)
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
        xs <- selectList [MenuTagMenuTagMenu ==. menuId] []
        oldTags <- fmap catMaybes $ forM xs $ \(Entity tagEntryId (MenuTag _ tagId)) ->
          liftIO (getMealTagById backend tagId)
        let toRemove = Set.fromList oldTags `Set.difference` Set.fromList menuSettingsTags
            toAdd = Set.fromList menuSettingsTags `Set.difference` Set.fromList oldTags
        forM_ toRemove $ \tag -> do
          mTagId <- liftIO (getMealTagId backend tag)
          case mTagId of
            Nothing -> pure ()
            Just tagId ->
              deleteWhere [MenuTagMenuTagMenu ==. menuId, MenuTagMenuTagMealTag ==. tagId]
        forM_ toAdd $ \tag -> do
          mTagId <- liftIO (getMealTagId backend tag)
          case mTagId of
            Nothing -> pure ()
            Just tagId ->
              insert_ (MenuTag menuId tagId)
        pure menuId

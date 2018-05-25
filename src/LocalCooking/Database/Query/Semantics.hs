module LocalCooking.Database.Query.Semantics where

import LocalCooking.Database.Schema.Semantics
  ( StoredChefId, StoredMealId, StoredMenuId
  , Unique (UniqueChefPermalink, UniqueMealPermalink, UniqueMenuDeadline)
  )

import Data.Text.Permalink (Permalink)
import Data.Time.Calendar (Day)
import Database.Persist (Entity (..), getBy)
import Database.Persist.Sql (ConnectionPool, runSqlPool)



getChefId :: ConnectionPool -> Permalink -> IO (Maybe StoredChefId)
getChefId backend permalink =
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

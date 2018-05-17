module LocalCooking.Database.Query.Tag.Meal where

import LocalCooking.Database.Schema.Tag.Meal
  ( StoredMealTag (..), Unique (..), StoredMealTagId )
import LocalCooking.Common.Tag.Meal (MealTag)

import Database.Persist (Entity (..), getBy, delete, insert_, selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)


insertMealTag :: ConnectionPool
              -> MealTag
              -> IO ()
insertMealTag backend tag =
  flip runSqlPool backend $
    insert_ (StoredMealTag tag)


deleteMealTag :: ConnectionPool
              -> MealTag
              -> IO ()
deleteMealTag backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueMealTag tag)
    case mEnt of
      Nothing -> pure ()
      Just (Entity k _) -> delete k


getMealTagId :: ConnectionPool
             -> MealTag
             -> IO (Maybe StoredMealTagId)
getMealTagId backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueMealTag tag)
    pure ((\(Entity k _) -> k) <$> mEnt)


getMealTags :: ConnectionPool
            -> IO [MealTag]
getMealTags backend =
  flip runSqlPool backend $ do
     xs <- selectList [] []
     pure $ (\(Entity _ (StoredMealTag x)) -> x) <$> xs

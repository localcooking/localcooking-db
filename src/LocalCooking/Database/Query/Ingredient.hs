module LocalCooking.Database.Query.Ingredient where

import LocalCooking.Database.Schema.Ingredient
  ( StoredIngredient (..), Unique (..), StoredIngredientId )
import LocalCooking.Common.Ingredient (Ingredient)

import Database.Persist (Entity (..), get, getBy, delete, insert_, selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)


insertIngredient :: ConnectionPool
                 -> Ingredient
                 -> IO ()
insertIngredient backend (Ingredient name voids) =
  flip runSqlPool backend $ do
    insert_ (StoredIngredient name)
    forM_ voids $ \d ->


deleteIngredient :: ConnectionPool
                 -> Ingredient
                 -> IO ()
deleteIngredient backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueIngredient tag)
    case mEnt of
      Nothing -> pure ()
      Just (Entity k _) -> delete k


getIngredientId :: ConnectionPool
                -> Ingredient
                -> IO (Maybe StoredIngredientId)
getIngredientId backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueIngredient tag)
    pure ((\(Entity k _) -> k) <$> mEnt)


getIngredientById :: ConnectionPool
               -> StoredIngredientId
               -> IO (Maybe Ingredient)
getIngredientById backend tagId =
  fmap (\(StoredIngredient t) -> t) <$> runSqlPool (get tagId) backend



getIngredients :: ConnectionPool
               -> IO [Ingredient]
getIngredients backend =
  flip runSqlPool backend $ do
     xs <- selectList [] []
     pure $ (\(Entity _ (StoredIngredient x)) -> x) <$> xs

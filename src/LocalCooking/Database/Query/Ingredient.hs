module LocalCooking.Database.Query.Ingredient where

import LocalCooking.Database.Query.Diet (getDietId)
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
    ingId <- insert (StoredIngredient name)
    ids <- forM_ voids $ \d -> liftIO (catMaybes <$> getDietId backend d)
    forM_ ids $ \i -> insert_ (IngredientDietViolation ingId i)



deleteIngredient :: ConnectionPool
                 -> IngredientName
                 -> IO ()
deleteIngredient backend name =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueIngredientName name)
    case mEnt of
      Nothing -> pure ()
      Just (Entity k _) -> do
        delete k
        -- FIXME diet and ingredient might have to be defined in the same module -
        -- deleting a diet doesn't remove linked ingredients.
        xs <- selectList [IngredientDietViolationIngredientViolator ==. k] []
        forM_ xs $ \(Entity k' _) -> delete k


getStoredIngredientId :: ConnectionPool
                      -> IngredientName
                      -> IO (Maybe StoredIngredientId)
getStoredIngredientId backend name =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueIngredient name)
    pure ((\(Entity k _) -> k) <$> mEnt)


getIngredientNameById :: ConnectionPool
                      -> StoredIngredientId
                      -> IO (Maybe IngredientName)
getIngredientNameById backend ingId =
  fmap (\(StoredIngredient t) -> t) <$> runSqlPool (get ingId) backend


getIngredientById :: ConnectionPool
                  -> StoredIngredientId
                  -> IO (Maybe Ingredient)
getIngredientById backend ingId = do
  mName <- liftIO (getIngredientNameById backend ingId)
  case mName of
    Nothing -> pure Nothing
    Just name -> do
      xs <- selectList [IngredientDietViolationIngredientViolator ==. ingId] []
      voids <- fmap catMaybes $ forM_ xs $ \(Entity _ (IngredientDietViolation _ d)) ->
        liftIO (getDietById backend d)
      pure (Ingredient name voids)

-- getIngredients :: ConnectionPool
--                -> IO [Ingredient]
-- getIngredients backend =
--   flip runSqlPool backend $ do
--      xs <- selectList [] []
--      pure $ (\(Entty _ (StoredIngredient x)) -> x) <$> xs

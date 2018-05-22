module LocalCooking.Database.Query.IngredientDiet where

import LocalCooking.Database.Schema.IngredientDiet
  ( StoredIngredient (..), Unique (..), StoredIngredientId, IngredientDietViolation (..), StoredDiet (..), StoredDietId
  , EntityField (IngredientDietViolationDietViolated, IngredientDietViolationIngredientViolator))
import LocalCooking.Common.Ingredient (Ingredient (..), IngredientName)
import LocalCooking.Common.Diet (Diet)

import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (catMaybes)
import Database.Persist (Entity (..), get, getBy, delete, insert, insert_, selectList, (==.))
import Database.Persist.Sql (ConnectionPool, runSqlPool)


insertIngredient :: ConnectionPool
                 -> Ingredient
                 -> IO ()
insertIngredient backend (Ingredient name voids) =
  flip runSqlPool backend $ do
    ingId <- insert (StoredIngredient name)
    ids <- fmap catMaybes $ forM voids $ \d -> liftIO (getDietId backend d)
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
        xs <- selectList [IngredientDietViolationIngredientViolator ==. k] []
        forM_ xs $ \(Entity k' _) -> delete k'


getStoredIngredientId :: ConnectionPool
                      -> IngredientName
                      -> IO (Maybe StoredIngredientId)
getStoredIngredientId backend name =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueIngredientName name)
    pure ((\(Entity k _) -> k) <$> mEnt)


getIngredientNameById :: ConnectionPool
                      -> StoredIngredientId
                      -> IO (Maybe IngredientName)
getIngredientNameById backend ingId =
  fmap (\(StoredIngredient t) -> t) <$> runSqlPool (get ingId) backend


getIngredientById :: ConnectionPool
                  -> StoredIngredientId
                  -> IO (Maybe Ingredient)
getIngredientById backend ingId =
  flip runSqlPool backend $ do
    mName <- liftIO (getIngredientNameById backend ingId)
    case mName of
      Nothing -> pure Nothing
      Just name -> do
        xs <- selectList [IngredientDietViolationIngredientViolator ==. ingId] []
        voids <- fmap catMaybes $ forM xs $ \(Entity _ (IngredientDietViolation _ d)) ->
          liftIO (getDietById backend d)
        pure $ Just $ Ingredient name voids

getIngredients :: ConnectionPool
               -> IO [Ingredient]
getIngredients backend =
  flip runSqlPool backend $ do
     xs <- selectList [] []
     fmap catMaybes $ forM xs $ \(Entity k _) ->
       liftIO (getIngredientById backend k)


insertDiet :: ConnectionPool
           -> Diet
           -> IO ()
insertDiet backend name =
  flip runSqlPool backend $
    insert_ (StoredDiet name)


deleteDiet :: ConnectionPool
           -> Diet
           -> IO ()
deleteDiet backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueDiet tag)
    case mEnt of
      Nothing -> pure ()
      Just (Entity k _) -> do
        delete k
        xs <- selectList [IngredientDietViolationDietViolated ==. k] []
        forM_ xs $ \(Entity k' _) -> delete k'


getDietId :: ConnectionPool
          -> Diet
          -> IO (Maybe StoredDietId)
getDietId backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueDiet tag)
    pure ((\(Entity k _) -> k) <$> mEnt)


getDietById :: ConnectionPool
            -> StoredDietId
            -> IO (Maybe Diet)
getDietById backend tagId =
  fmap (\(StoredDiet t) -> t) <$> runSqlPool (get tagId) backend



getDiets :: ConnectionPool
         -> IO [Diet]
getDiets backend =
  flip runSqlPool backend $ do
    xs <- selectList [] []
    pure $ (\(Entity _ (StoredDiet x)) -> x) <$> xs

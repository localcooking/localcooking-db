module LocalCooking.Database.Query.IngredientDiet where

import LocalCooking.Database.Schema.IngredientDiet
  ( StoredIngredient (..), Unique (..), StoredIngredientId, IngredientDietViolation (..), StoredDiet (..), StoredDietId
  , EntityField (IngredientDietViolationDietViolated, IngredientDietViolationIngredientViolator))
import LocalCooking.Common.Ingredient (Ingredient (..), IngredientName)
import LocalCooking.Common.Diet (Diet)

import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Control.Monad (forM_, forM, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Persist (Entity (..), get, getBy, delete, deleteWhere, insert_, selectList, (==.))
import Database.Persist.Sql (ConnectionPool, runSqlPool)


insertIngredient :: ConnectionPool
                 -> Ingredient
                 -> IO ()
insertIngredient backend (Ingredient name voids) =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueIngredientName name)
    case mEnt of
      Nothing -> do
        insert_ (StoredIngredient name)
        void $ liftIO $ setViolations backend name voids
      Just _ ->
        void $ liftIO $ setViolations backend name voids



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


getIngredientViolations :: ConnectionPool
                        -> StoredIngredientId
                        -> IO [Diet]
getIngredientViolations backend ingId =
  flip runSqlPool backend $ do
    xs <- selectList [IngredientDietViolationIngredientViolator ==. ingId] []
    fmap catMaybes $ forM xs $ \(Entity _ (IngredientDietViolation _ d)) ->
      liftIO (getDietById backend d)


getIngredientById :: ConnectionPool
                  -> StoredIngredientId
                  -> IO (Maybe Ingredient)
getIngredientById backend ingId =
  flip runSqlPool backend $ do
    mName <- liftIO (getIngredientNameById backend ingId)
    case mName of
      Nothing -> pure Nothing
      Just name -> do
        voids <- liftIO (getIngredientViolations backend ingId)
        pure $ Just $ Ingredient name voids


getIngredientByName :: ConnectionPool
                    -> IngredientName
                    -> IO (Maybe Ingredient)
getIngredientByName backend ingName = do
  mIngId <- getStoredIngredientId backend ingName
  case mIngId of
    Nothing -> pure Nothing
    Just ingId -> getIngredientById backend ingId


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


registerViolation :: ConnectionPool
                  -> IngredientName
                  -> Diet
                  -> IO Bool
registerViolation backend name diet =
  flip runSqlPool backend $ do
    mIngId <- liftIO (getStoredIngredientId backend name)
    mDietId <- liftIO (getDietId backend diet)
    case (,) <$> mIngId <*> mDietId of
      Nothing -> pure False
      Just (ingId,dietId) -> do
        insert_ (IngredientDietViolation ingId dietId)
        pure True


setViolations :: ConnectionPool
              -> IngredientName
              -> [Diet]
              -> IO Bool
setViolations backend name diets =
  flip runSqlPool backend $ do
    mIngId <- liftIO (getStoredIngredientId backend name)
    case mIngId of
      Nothing -> pure False
      Just ingId -> do
        oldDietIds <- fmap (fmap (\(Entity _ (IngredientDietViolation _ k)) -> k))
                    $ selectList [IngredientDietViolationIngredientViolator ==. ingId] []
        newDietIds <- fmap catMaybes $ forM diets $ liftIO . getDietId backend
        let toRemove = Set.fromList oldDietIds `Set.difference` Set.fromList newDietIds
            toAdd = Set.fromList newDietIds `Set.difference` Set.fromList oldDietIds
        forM_ toRemove $ \dietId -> deleteWhere
          [ IngredientDietViolationIngredientViolator ==. ingId
          , IngredientDietViolationDietViolated ==. dietId
          ]
        forM_ toAdd $ \dietId -> insert_ (IngredientDietViolation ingId dietId)
        pure True


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

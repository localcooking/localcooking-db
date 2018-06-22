module LocalCooking.Database.Query.IngredientDiet where

import LocalCooking.Database.Schema.IngredientDiet
  ( Unique (UniqueViolation), IngredientDietViolation (..)
  , EntityField (IngredientDietViolationDietViolated, IngredientDietViolationIngredientViolator))
import LocalCooking.Database.Schema.Tag.Ingredient
  ( Unique (UniqueStoredIngredientTag), StoredIngredientTag (..), StoredIngredientTagId)
import LocalCooking.Database.Schema.Tag.Diet
  ( Unique (UniqueStoredDietTag), StoredDietTag (..), StoredDietTagId)
import LocalCooking.Common.Ingredient (Ingredient (..))
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)

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
    mEnt <- getBy (UniqueStoredIngredientTag name)
    case mEnt of
      Nothing -> do
        insert_ (StoredIngredientTag name)
        void $ liftIO $ setViolations backend name voids
      Just _ ->
        void $ liftIO $ setViolations backend name voids



deleteIngredient :: ConnectionPool
                 -> IngredientTag
                 -> IO ()
deleteIngredient backend name =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueStoredIngredientTag name)
    case mEnt of
      Nothing -> pure ()
      Just (Entity k _) -> do
        delete k
        deleteWhere [IngredientDietViolationIngredientViolator ==. k]


getStoredIngredientTagId :: ConnectionPool
                         -> IngredientTag
                         -> IO (Maybe StoredIngredientTagId)
getStoredIngredientTagId backend name =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueStoredIngredientTag name)
    pure ((\(Entity k _) -> k) <$> mEnt)


getIngredientTagById :: ConnectionPool
                     -> StoredIngredientTagId
                     -> IO (Maybe IngredientTag)
getIngredientTagById backend ingId =
  fmap (\(StoredIngredientTag t) -> t) <$> runSqlPool (get ingId) backend


getIngredientViolations :: ConnectionPool
                        -> StoredIngredientTagId
                        -> IO [DietTag]
getIngredientViolations backend ingId =
  flip runSqlPool backend $ do
    xs <- selectList [IngredientDietViolationIngredientViolator ==. ingId] []
    fmap catMaybes $ forM xs $ \(Entity _ (IngredientDietViolation _ d)) ->
      liftIO (getDietById backend d)


getIngredientById :: ConnectionPool
                  -> StoredIngredientTagId
                  -> IO (Maybe Ingredient)
getIngredientById backend ingId =
  flip runSqlPool backend $ do
    mName <- liftIO (getIngredientTagById backend ingId)
    case mName of
      Nothing -> pure Nothing
      Just name -> do
        voids <- liftIO (getIngredientViolations backend ingId)
        pure $ Just $ Ingredient name voids


getIngredientByName :: ConnectionPool
                    -> IngredientTag
                    -> IO (Maybe Ingredient)
getIngredientByName backend ingName = do
  mIngId <- getStoredIngredientTagId backend ingName
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


insertDietTag :: ConnectionPool
              -> DietTag
              -> IO ()
insertDietTag backend name =
  flip runSqlPool backend $
    insert_ (StoredDietTag name)


registerViolation :: ConnectionPool
                  -> IngredientTag
                  -> DietTag
                  -> IO Bool
registerViolation backend name diet =
  flip runSqlPool backend $ do
    mIngId <- liftIO (getStoredIngredientTagId backend name)
    mDietId <- liftIO (getDietId backend diet)
    case (,) <$> mIngId <*> mDietId of
      Nothing -> pure False
      Just (ingId,dietId) -> do
        insert_ (IngredientDietViolation ingId dietId)
        pure True


setViolations :: ConnectionPool
              -> IngredientTag
              -> [DietTag]
              -> IO Bool
setViolations backend name diets =
  flip runSqlPool backend $ do
    mIngId <- liftIO (getStoredIngredientTagId backend name)
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


deleteDietTag :: ConnectionPool
              -> DietTag
              -> IO ()
deleteDietTag backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueStoredDietTag tag)
    case mEnt of
      Nothing -> pure ()
      Just (Entity k _) -> do
        delete k
        xs <- selectList [IngredientDietViolationDietViolated ==. k] []
        forM_ xs $ \(Entity k' _) -> delete k'


getDietId :: ConnectionPool
          -> DietTag
          -> IO (Maybe StoredDietTagId)
getDietId backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueStoredDietTag tag)
    pure ((\(Entity k _) -> k) <$> mEnt)


getDietById :: ConnectionPool
            -> StoredDietTagId
            -> IO (Maybe DietTag)
getDietById backend tagId =
  fmap (\(StoredDietTag t) -> t) <$> runSqlPool (get tagId) backend



getDiets :: ConnectionPool
         -> IO [DietTag]
getDiets backend =
  flip runSqlPool backend $ do
    xs <- selectList [] []
    pure $ (\(Entity _ (StoredDietTag x)) -> x) <$> xs

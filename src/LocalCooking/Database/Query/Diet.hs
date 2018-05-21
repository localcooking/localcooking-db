module LocalCooking.Database.Query.Diet where

import LocalCooking.Database.Schema.Diet
  ( StoredDiet (..), Unique (..), StoredDietId )
import LocalCooking.Common.Diet (Diet)

import Database.Persist (Entity (..), get, getBy, delete, insert_, selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)



insertDiet :: ConnectionPool
           -> Diet
           -> IO ()
insertDiet backend (Diet name) =
  flip runSqlPool backend $ do
    insert_ (StoredDiet name)


deleteDiet :: ConnectionPool
           -> Diet
           -> IO ()
deleteDiet backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueDiet tag)
    case mEnt of
      Nothing -> pure ()
      Just (Entity k _) -> delete k


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

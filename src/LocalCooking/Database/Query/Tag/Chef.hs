module LocalCooking.Database.Query.Tag.Chef where

import LocalCooking.Database.Schema.Tag.Chef
  ( StoredChefTag (..), Unique (..), StoredChefTagId )
import LocalCooking.Common.Tag.Chef (ChefTag)

import Database.Persist (Entity (..), get, getBy, delete, insert_, selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)


insertChefTag :: ConnectionPool
              -> ChefTag
              -> IO ()
insertChefTag backend tag =
  flip runSqlPool backend $
    insert_ (StoredChefTag tag)


deleteChefTag :: ConnectionPool
              -> ChefTag
              -> IO ()
deleteChefTag backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueChefTag tag)
    case mEnt of
      Nothing -> pure ()
      Just (Entity k _) -> delete k


getChefTagId :: ConnectionPool
             -> ChefTag
             -> IO (Maybe StoredChefTagId)
getChefTagId backend tag =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueChefTag tag)
    pure ((\(Entity k _) -> k) <$> mEnt)


getChefTagById :: ConnectionPool
               -> StoredChefTagId
               -> IO (Maybe ChefTag)
getChefTagById backend tagId =
  fmap (\(StoredChefTag t) -> t) <$> runSqlPool (get tagId) backend


getChefTags :: ConnectionPool
            -> IO [ChefTag]
getChefTags backend =
  flip runSqlPool backend $ do
     xs <- selectList [] []
     pure $ (\(Entity _ (StoredChefTag x)) -> x) <$> xs

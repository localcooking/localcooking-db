{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , DeriveGeneric
  , RecordWildCards
  #-}

module LocalCooking.Database.Query.Semantics.Admin where

-- import LocalCooking.Database.Query.Ingredient (getIngredientId, getIngredientById)
import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Schema.User.Role (UserRoleStored (..), EntityField (UserRoleStoredUserRoleOwner), Unique (UniqueUserRole))
import LocalCooking.Common.User.Role (UserRole)

import Database.Persist
  ( Entity (..)
  , insert_
  , delete
  , getBy
  , (==.)
  , selectList
  )
import Database.Persist.Sql (ConnectionPool, runSqlPool)


addRole :: ConnectionPool
        -> StoredUserId
        -> UserRole
        -> IO ()
addRole backend userId userRole =
  flip runSqlPool backend $ do
    mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
    case mUserRoleEnt of
      Just _ -> pure ()
      Nothing -> insert_ (UserRoleStored userRole userId)


delRole :: ConnectionPool
        -> StoredUserId
        -> UserRole
        -> IO ()
delRole backend userId userRole =
  flip runSqlPool backend $ do
    mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
    case mUserRoleEnt of
      Nothing -> pure ()
      Just (Entity userRoleKey _) -> delete userRoleKey


hasRole :: ConnectionPool
        -> StoredUserId
        -> UserRole
        -> IO Bool
hasRole backend userId userRole =
  flip runSqlPool backend $ do
    mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
    case mUserRoleEnt of
      Nothing -> pure False
      Just _  -> pure True


getRoles :: ConnectionPool
         -> StoredUserId
         -> IO [UserRole]
getRoles backend userId =
  flip runSqlPool backend $ do
    userRoleEnts <- selectList [UserRoleStoredUserRoleOwner ==. userId] []
    pure ((\(Entity _ (UserRoleStored x _)) -> x) <$> userRoleEnts)

module LocalCooking.Database.Schema where

import qualified LocalCooking.Database.Schema.Facebook.AccessToken as FacebookAccess
import qualified LocalCooking.Database.Schema.Facebook.UserDetails as FacebookDetails
import qualified LocalCooking.Database.Schema.Image as Image
import qualified LocalCooking.Database.Schema.IngredientDiet as IngredientDiet
import qualified LocalCooking.Database.Schema.Semantics as Semantics
import qualified LocalCooking.Database.Schema.Tag.Meal as TagMeal
import qualified LocalCooking.Database.Schema.Tag.Chef as TagChef
import qualified LocalCooking.Database.Schema.User as User
import qualified LocalCooking.Database.Schema.User.Customer as Customer
import qualified LocalCooking.Database.Schema.User.Role as UserRole
import qualified LocalCooking.Database.Schema.Salt as Salt

import Database.Persist.Sql (ConnectionPool, runSqlPool, runMigration)



migrateAll :: ConnectionPool -> IO ()
migrateAll backend = do
  flip runSqlPool backend $ do
    runMigration Salt.migrateAll
    runMigration Image.migrateAll
    runMigration IngredientDiet.migrateAll
    runMigration TagMeal.migrateAll
    runMigration TagChef.migrateAll
    runMigration User.migrateAll
    runMigration UserRole.migrateAll
    runMigration Customer.migrateAll
    runMigration FacebookDetails.migrateAll
    runMigration FacebookAccess.migrateAll
    runMigration Semantics.migrateAll

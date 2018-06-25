module LocalCooking.Database.Schema.Init where

import qualified LocalCooking.Database.Schema as Schema
import qualified LocalCooking.Database.Schema.Content as ContentSchema

import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Class (insert_, getBy)


migrateEverything :: ReaderT SqlBackend IO ()
migrateEverything = do
  runMigration Schema.migrateAll
  runMigration ContentSchema.migrateAll
  -- initialize total coverage of all submittable policy variants
  forM_ [minBound .. maxBound] $ \variant -> do
    mPolicy <- getBy (ContentSchema.UniqueSubmissionPolicyVariant variant)
    case mPolicy of
      Just _ -> pure ()
      Nothing -> insert_ (ContentSchema.RecordSubmissionPolicy variant 1)

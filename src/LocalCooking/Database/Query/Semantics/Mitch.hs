{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , DeriveGeneric
  , RecordWildCards
  #-}

module LocalCooking.Database.Query.Semantics.Mitch where

-- import LocalCooking.Database.Query.Ingredient (getIngredientId, getIngredientById)
-- import LocalCooking.Database.Query.Tag.Meal (getMealTagId, getMealTagById)
-- import LocalCooking.Database.Schema.Semantics (StoredMeal (..), MealIngredient (..), MealTag (..), EntityField (..))
-- import LocalCooking.Semantics.Mitch (MealSynopsis (..), Meal (..), Chef (..))

-- import Data.Text.Permalink (Permalink)
-- import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
-- import Data.Aeson.Types (typeMismatch)
-- import Data.Maybe (catMaybes)
-- import qualified Data.Set as Set
-- import Control.Monad (forM, forM_)
-- import Control.Monad.IO.Class (liftIO)
-- import Text.EmailAddress (EmailAddress)
-- import Database.Persist (Entity (..), insert, insert_, delete, deleteBy, get, getBy, (=.), update, (==.), selectList)
-- import Database.Persist.Sql (ConnectionPool, runSqlPool)
-- import GHC.Generics (Generic)
-- import Test.QuickCheck (Arbitrary (..), oneof)


-- getChef :: ConnectionPool
--         -> Permalink
--         -> IO (Maybe Chef)
-- getChef backend chefId =
--   flip runSqlPool backend $ do
--     mEnt <- getBy (UniqueChefPermalink chefId)
--     case mEnt of
--       Nothing -> pure Nothing
--       Just (Entity storedChefId (StoredChef ownerId name _ bio images avatar)) -> do
--         pure $ Just $ Chef
--           { chefName = name
--           , chefPermalink = chefId
--           , chefImages = images
--           , chefBio = bio
--           , chefRating = _
--           , chefReviews = _
--           , chefActiveOrders = _
--           , chefTotalOrders = _
--           , chefTags = _
--           , chefMenus = _
--           }

{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , DeriveGeneric
  , RecordWildCards
  #-}

module LocalCooking.Database.Query.Semantics.Chef where

-- import LocalCooking.Database.Query.Ingredient (getIngredientId, getIngredientById)
import LocalCooking.Database.Query.Tag.Chef (getChefTagId, getChefTagById)
import LocalCooking.Database.Schema.Semantics (StoredChef (..), ChefTag (..), EntityField (..), Unique (UniqueChefPermalink))
import LocalCooking.Database.Schema.User (UserId)
import LocalCooking.Semantic.Chef (MealSettings (..), MenuSettings (..), ChefSettings (..))

import Data.Text.Permalink (Permalink)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Text.EmailAddress (EmailAddress)
import Database.Persist (Entity (..), insert, insert_, delete, deleteBy, get, getBy, (=.), update, (==.), selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), oneof)


setChef :: ConnectionPool
        -> UserId
        -> ChefSettings
        -> IO Bool
setChef backend owner ChefSettings{..} =
  flip runSqlPool backend $ do
    mEnt <- getBy (UniqueChefPermalink chefSettingsPermalink)
    case mEnt of
      Just _ -> pure False
      Nothing -> do
        chefId <- insert $ StoredChef
                    owner
                    chefSettingsName
                    chefSettingsPermalink
                    chefSettingsBio
                    chefSettingsImages
                    chefSettingsAvatar
        forM_ chefSettingsTags $ \tag -> do
          mId <- liftIO (getChefTagId backend tag)
          case mId of
            Nothing -> pure ()
            Just tagId ->
              insert_ (ChefTag chefId tagId)
        pure True

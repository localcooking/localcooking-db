{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Ingredient where

import LocalCooking.Common.Ingredient (Ingredient)

import Data.Hashable (Hashable (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistEntity (EntityField, Key))
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredIngredient
    ingredient Ingredient
    UniqueIngredient ingredient
    deriving Eq Show
|]

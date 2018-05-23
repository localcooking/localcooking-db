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

module LocalCooking.Database.Schema.IngredientDiet where

import LocalCooking.Common.Ingredient (IngredientName)
import LocalCooking.Common.Diet (Diet)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


-- Globally, there will only be one stored ingredient name, and one stored
-- diet name. A member of the IngredientDietViolation record represents
-- the unique _relation_ between the two entities - there could be many records
-- of this relation using similar diets or ingredients.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredIngredient
    ingredientName IngredientName
    UniqueIngredientName ingredientName
    deriving Eq Show

StoredDiet
    diet Diet
    UniqueDiet diet
    deriving Eq Show

IngredientDietViolation
    ingredientViolator StoredIngredientId
    dietViolated StoredDietId
    UniqueViolation ingredientViolator dietViolated
    deriving Eq Show
|]

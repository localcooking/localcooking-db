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

import LocalCooking.Database.Schema.Tag.Ingredient (StoredIngredientTagId)
import LocalCooking.Database.Schema.Tag.Diet (StoredDietTagId)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


-- Globally, there will only be one stored ingredient name, and one stored
-- diet name. A member of the IngredientDietViolation record represents
-- the unique _relation_ between the two entities - there could be many records
-- of this relation using similar diets or ingredients.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
IngredientDietViolation
    ingredientViolator StoredIngredientTagId
    dietViolated StoredDietTagId
    UniqueViolation ingredientViolator dietViolated
    deriving Eq Show
|]

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

module LocalCooking.Database.Schema.User.Customer where

import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Schema.Tag.Ingredient (StoredIngredientTagId)
import LocalCooking.Database.Schema.Tag.Diet (StoredDietTagId)
import LocalCooking.Common.User.Name (Name)

import Data.Address (USAAddress)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredCustomer
    storedCustomerOwner StoredUserId
    storedCustomerName Name
    storedCustomerAddress USAAddress
    UniqueCustomer storedCustomerOwner
    deriving Eq Show

StoredDietPreference
    dietPreferenceOwner StoredCustomerId
    dietPreferenceDiet  StoredDietTagId
    UniqueDietPreference dietPreferenceOwner dietPreferenceDiet
    deriving Eq Show

StoredAllergy
    allergyOwner StoredCustomerId
    allergy      StoredIngredientTagId
    UniqueAllergy allergyOwner allergy
    deriving Eq Show
|]

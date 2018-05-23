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
import LocalCooking.Database.Schema.IngredientDiet (StoredDietId, StoredIngredientId)
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
    dietPreferenceDiet  StoredDietId
    UniqueDietPreference dietPreferenceOwner dietPreferenceDiet
    deriving Eq Show

StoredAllergy
    allergyOwner StoredCustomerId
    allergy      StoredIngredientId
    UniqueAllergy allergyOwner allergy
    deriving Eq Show
|]

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

module LocalCooking.Database.Schema.User.Editor where

import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Common.User.Name (Name)

import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredEditor
    storedEditorOwner StoredUserId
    storedEditorName Name
    UniqueEditor storedEditorOwner
    deriving Eq Show
|]

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

import Database.Persist.Sql (toSqlKey)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Test.QuickCheck (Arbitrary (..))


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredEditor
    storedEditorOwner StoredUserId
    storedEditorName Name
    UniqueEditor storedEditorOwner
    deriving Eq Show
|]

instance Arbitrary StoredEditorId where
  arbitrary = toSqlKey <$> arbitrary

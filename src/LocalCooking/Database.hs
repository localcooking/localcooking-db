{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  , MultiParamTypeClasses
  , GADTs
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database where


import Database.Persist ()
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email String
    password String
    deriving Show
|]

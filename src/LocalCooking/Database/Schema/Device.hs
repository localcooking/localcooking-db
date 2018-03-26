{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.Device where

import LocalCooking.Common.DeviceToken (DeviceToken)

import Data.Text (Text)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- stored device tokens, for book-keeping
RegisteredDeviceToken
    deviceToken DeviceToken
    UniqueDeviceToken deviceToken
    deriving Eq Show

DeviceDescription
    deviceName Text
    deviceTokenOwner RegisteredDeviceTokenId
    DeviceDescriptionOwner deviceTokenOwner
    deriving Eq Show
|]

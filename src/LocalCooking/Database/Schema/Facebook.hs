module LocalCooking.Database.Schema.Facebook
  ( module LocalCooking.Database.Schema.Facebook.AccessToken
  , module LocalCooking.Database.Schema.Facebook.UserDetails
  ) where

import LocalCooking.Database.Schema.Facebook.AccessToken hiding (migrateAll)
import LocalCooking.Database.Schema.Facebook.UserDetails hiding (migrateAll)

module LocalCooking.Database.Query.Salt where

import LocalCooking.Database.Schema.Salt
  ( PasswordSalt (..)
  )
import LocalCooking.Common.Password (HashedPassword (..))

import Database.Persist (Entity (..), insert_, selectFirst)
import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Crypto.Saltine.Core.Utils (randomByteString)



getPasswordSalt :: ConnectionPool
                -> IO HashedPassword
getPasswordSalt backend = do
  newSalt <- HashedPassword <$> randomByteString 32
  flip runSqlPool backend $ do
    mSalt <- selectFirst [] []
    case mSalt of
      Nothing -> do
        insert_ (PasswordSalt newSalt)
        pure newSalt
      Just (Entity _ (PasswordSalt salt)) ->
        pure salt

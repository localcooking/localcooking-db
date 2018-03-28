module LocalCooking.Database.Query.User where

import LocalCooking.Database.Schema.Facebook
  ( FacebookUserDetailsId, FacebookUserDetails (..), FacebookUserAccessTokenStored (..)
  , Unique (..)
  )
import LocalCooking.Database.Schema.User
  ( User (..), EmailAddressStored (..), UserId
  , Unique (..)
  )
import LocalCooking.Database.Schema.Auth
  ( RegisteredAuthToken (..)
  )
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AuthToken (AuthToken, genAuthToken)
import Facebook.User (FacebookUserId, FacebookUserAccessToken)

import Data.List.NonEmpty (NonEmpty (..))
import Control.Monad (forM_)
import Text.EmailAddress (EmailAddress)
import Database.Persist (Entity (..), insert, insert_, delete, get, getBy)
import Database.Persist.Sql (ConnectionPool, runSqlPool)



registerUser :: ConnectionPool
             -> NonEmpty EmailAddress
             -> HashedPassword
             -> IO UserId
registerUser backend (e:|es) password =
  flip runSqlPool backend $ do
    userId <- insert $ User password
    forM_ (e:es) $ \e' ->
      insert $ EmailAddressStored e' userId
    pure userId


registerFBUserId :: ConnectionPool
                 -> UserId
                 -> FacebookUserId
                 -> IO ()
registerFBUserId backend userId fbUserId =
  flip runSqlPool backend $
    insert_ $ FacebookUserDetails fbUserId userId


login :: ConnectionPool
      -> EmailAddress
      -> HashedPassword
      -> IO (Maybe AuthToken)
login backend email password = do
  authToken <- genAuthToken
  flip runSqlPool backend $ do
    mEmail <- getBy $ UniqueEmailAddress email
    case mEmail of
      Nothing -> pure Nothing
      Just (Entity email' (EmailAddressStored _ owner)) -> do
        mUser <- get owner
        case mUser of
          Nothing -> do
            -- clean-up:
            delete email'
            pure Nothing
          Just (User password')
            | password == password' -> pure (Just authToken)
            | otherwise -> pure Nothing


-- | NOTE: Doesn't verify the authenticity of FacebookUserAccessToken
loginWithFB :: ConnectionPool
            -> FacebookUserAccessToken
            -> FacebookUserId
            -> IO (Maybe AuthToken)
loginWithFB backend fbToken fbUserId = do
  authToken <- genAuthToken
  flip runSqlPool backend $ do
    mDetails <- getBy $ UniqueFacebookUserId fbUserId
    case mDetails of
      Nothing -> pure Nothing
      Just (Entity fbUserIdId (FacebookUserDetails _ userId)) -> do
        insert_ $ FacebookUserAccessTokenStored fbToken fbUserIdId
        insert_ $ RegisteredAuthToken authToken userId
        pure (Just authToken)

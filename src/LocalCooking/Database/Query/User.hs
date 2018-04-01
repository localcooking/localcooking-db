{-# LANGUAGE
    OverloadedStrings
  #-}

module LocalCooking.Database.Query.User where

import LocalCooking.Database.Schema.Facebook
  ( FacebookUserDetailsId, FacebookUserDetails (..), FacebookUserAccessTokenStored (..)
  , Unique (..)
  )
import LocalCooking.Database.Schema.User
  ( User (..), EmailAddressStored (..), UserId, PendingRegistrationConfirm (..)
  , Unique (..)
  )
import LocalCooking.Database.Schema.Auth
  ( RegisteredAuthToken (..)
  , Unique (..)
  )
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AuthToken (AuthToken, genAuthToken)
import Facebook.Types (FacebookUserId, FacebookUserAccessToken)

import Data.Aeson (ToJSON (..), Value (String))
import Data.List.NonEmpty (NonEmpty (..))
import Control.Monad (forM_)
import Text.EmailAddress (EmailAddress)
import Database.Persist (Entity (..), insert, insert_, delete, get, getBy)
import Database.Persist.Sql (ConnectionPool, runSqlPool)



data RegisterFailure
  = EmailExists
  deriving (Eq, Show)

instance ToJSON RegisterFailure where
  toJSON x = String $ case x of
    EmailExists -> "email-exists"


registerUser :: ConnectionPool
             -> EmailAddress
             -> HashedPassword
             -> IO (Either RegisterFailure UserId)
registerUser backend email password =
  flip runSqlPool backend $ do
    mEnt <- getBy $ UniqueEmailAddress email
    case mEnt of
      Just _ -> pure (Left EmailExists)
      Nothing -> do
        userId <- insert $ User password
        insert_ $ EmailAddressStored email userId
        insert_ $ PendingRegistrationConfirm userId
        pure (Right userId)


confirmEmail :: ConnectionPool
             -> EmailAddress
             -> IO Bool
confirmEmail backend email =
  flip runSqlPool backend $ do
    mUserEnt <- getBy $ UniqueEmailAddress email
    case mUserEnt of
      Nothing -> pure False
      Just (Entity _ (EmailAddressStored _ owner)) -> do
        mPendingEnt <- getBy $ UniquePendingRegistration owner
        case mPendingEnt of
          Nothing -> pure False
          Just (Entity pendingKey _) -> do
            delete pendingKey
            pure True


registerFBUserId :: ConnectionPool
                 -> UserId
                 -> FacebookUserId
                 -> IO ()
registerFBUserId backend userId fbUserId =
  flip runSqlPool backend $
    insert_ $ FacebookUserDetails fbUserId userId


data AuthTokenFailure
  = BadPassword
  | EmailDoesntExist
  deriving (Eq, Show)

instance ToJSON AuthTokenFailure where
  toJSON x = String $ case x of
    BadPassword -> "bad-password"
    EmailDoesntExist -> "no-email"


login :: ConnectionPool
      -> EmailAddress
      -> HashedPassword
      -> IO (Either AuthTokenFailure AuthToken)
login backend email password = do
  authToken <- genAuthToken
  flip runSqlPool backend $ do
    mEmail <- getBy $ UniqueEmailAddress email
    case mEmail of
      Nothing -> pure (Left EmailDoesntExist)
      Just (Entity email' (EmailAddressStored _ owner)) -> do
        -- no need to check for pending email here - only when filing orders, stuff like that
        mUser <- get owner
        case mUser of
          Nothing -> do
            -- clean-up:
            delete email'
            pure (Left EmailDoesntExist)
          Just (User password')
            | password == password' -> do
                insert_ $ RegisteredAuthToken authToken owner
                pure (Right authToken)
            | otherwise -> pure (Left BadPassword)


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


usersAuthToken :: ConnectionPool
               -> AuthToken
               -> IO (Maybe UserId)
usersAuthToken backend authToken =
  flip runSqlPool backend $ do
    mRegistered <- getBy $ UniqueAuthToken authToken
    case mRegistered of
      Nothing -> pure Nothing
      Just (Entity _ (RegisteredAuthToken _ userId)) -> pure (Just userId)


logout :: ConnectionPool
       -> AuthToken
       -> IO ()
logout backend authToken =
  flip runSqlPool backend $ do
    mRegistered <- getBy $ UniqueAuthToken authToken
    case mRegistered of
      Nothing -> pure ()
      Just (Entity tokenId _) -> delete tokenId

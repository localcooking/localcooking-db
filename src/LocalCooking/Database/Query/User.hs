{-# LANGUAGE
    OverloadedStrings
  #-}

module LocalCooking.Database.Query.User where

import LocalCooking.Database.Schema.Facebook
  ( FacebookUserDetails (..), FacebookUserAccessTokenStored (..)
  , Unique (..)
  )
import LocalCooking.Database.Schema.User
  ( User (..), EmailAddressStored (..), UserId, PendingRegistrationConfirm (..)
  , Unique (..)
  )
import LocalCooking.Common.Password (HashedPassword)
import Facebook.Types (FacebookUserId, FacebookUserAccessToken)

import Data.Aeson (ToJSON (..), Value (String))
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


getEmail :: ConnectionPool
         -> UserId
         -> IO (Maybe EmailAddress)
getEmail backend owner =
  flip runSqlPool backend $ do
    mEmailEnt <- getBy $ EmailAddressOwner owner
    case mEmailEnt of
      Nothing -> pure Nothing
      Just (Entity _ (EmailAddressStored email _)) -> pure (Just email)


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


-- | Doesn't write to database, read-only query
login :: ConnectionPool
      -> EmailAddress
      -> HashedPassword
      -> IO (Maybe AuthTokenFailure)
login backend email password =
  flip runSqlPool backend $ do
    mEmail <- getBy $ UniqueEmailAddress email
    case mEmail of
      Nothing -> pure (Just EmailDoesntExist)
      Just (Entity email' (EmailAddressStored _ owner)) -> do
        -- no need to check for pending email here - only when filing orders, stuff like that
        mUser <- get owner
        case mUser of
          Nothing -> do
            -- clean-up:
            delete email'
            pure (Just EmailDoesntExist)
          Just (User password')
            | password == password' -> pure Nothing
            | otherwise -> pure (Just BadPassword)


-- | NOTE: Doesn't verify the authenticity of FacebookUserAccessToken, but stores it
loginWithFB :: ConnectionPool
            -> FacebookUserAccessToken
            -> FacebookUserId
            -> IO (Maybe UserId)
loginWithFB backend fbToken fbUserId =
  flip runSqlPool backend $ do
    mDetails <- getBy $ UniqueFacebookUserId fbUserId
    case mDetails of
      Nothing -> pure Nothing
      Just (Entity fbUserIdId (FacebookUserDetails _ owner)) -> do
        insert_ $ FacebookUserAccessTokenStored fbToken fbUserIdId
        pure (Just owner)

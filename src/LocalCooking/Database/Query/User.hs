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
  , EntityField (EmailAddressStoredEmailAddress, UserRoleStoredUserRole, UserRoleStoredUserRoleOwner, UserPassword)
  , UserRoleStored (..)
  )
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole)
import Facebook.Types (FacebookUserId, FacebookUserAccessToken)

import Data.Aeson (ToJSON (..), Value (String))
import Text.EmailAddress (EmailAddress)
import Database.Persist (Entity (..), insert, insert_, delete, get, getBy, (=.), update, (==.), selectList)
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


userIdByEmail :: ConnectionPool
              -> EmailAddress
              -> IO (Maybe UserId)
userIdByEmail backend email =
  flip runSqlPool backend $ do
    mEmailEnt <- getBy $ UniqueEmailAddress email
    case mEmailEnt of
      Nothing -> pure Nothing
      Just (Entity _ (EmailAddressStored _ userId)) -> pure (Just userId)


registerFBUserId :: ConnectionPool
                 -> UserId
                 -> FacebookUserId
                 -> IO ()
registerFBUserId backend userId fbUserId =
  flip runSqlPool backend $
    insert_ $ FacebookUserDetails fbUserId userId


data LoginFailure
  = BadPassword
  | EmailDoesntExist
  deriving (Eq, Show)

instance ToJSON LoginFailure where
  toJSON x = String $ case x of
    BadPassword -> "bad-password"
    EmailDoesntExist -> "no-email"


-- | Doesn't write to database, read-only query
login :: ConnectionPool
      -> EmailAddress
      -> HashedPassword
      -> IO (Either LoginFailure UserId)
login backend email password =
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
            | password == password' -> pure (Right owner)
            | otherwise -> pure (Left BadPassword)


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


changeSecurityDetails :: ConnectionPool
                      -> UserId
                      -> (EmailAddress, HashedPassword)
                      -> HashedPassword
                      -> IO Bool
changeSecurityDetails backend userId (email,newPassword) password =
  flip runSqlPool backend $ do
    mUser <- get userId
    case mUser of
      Nothing -> pure False
      Just (User password')
        | password' /= password -> pure False
        | otherwise -> do
            mEmail <- getBy $ EmailAddressOwner userId
            case mEmail of
              Nothing -> insert_ (EmailAddressStored email userId)
              Just (Entity emailKey _) -> update emailKey [EmailAddressStoredEmailAddress =. email]
            update userId [UserPassword =. newPassword]
            pure True


checkPassword :: ConnectionPool
              -> UserId
              -> HashedPassword
              -> IO Bool
checkPassword backend userId password =
  flip runSqlPool backend $ do
    mUser <- get userId
    case mUser of
      Nothing -> pure False
      Just (User password') -> pure (password == password')



addRole :: ConnectionPool
        -> UserId
        -> UserRole
        -> IO ()
addRole backend userId userRole =
  flip runSqlPool backend $ do
    mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
    case mUserRoleEnt of
      Just _ -> pure ()
      Nothing -> insert_ (UserRoleStored userRole userId)


delRole :: ConnectionPool
        -> UserId
        -> UserRole
        -> IO ()
delRole backend userId userRole =
  flip runSqlPool backend $ do
    mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
    case mUserRoleEnt of
      Nothing -> pure ()
      Just (Entity userRoleKey _) -> delete userRoleKey


hasRole :: ConnectionPool
        -> UserId
        -> UserRole
        -> IO Bool
hasRole backend userId userRole =
  flip runSqlPool backend $ do
    mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
    case mUserRoleEnt of
      Nothing -> pure False
      Just _  -> pure True


getRoles :: ConnectionPool
         -> UserId
         -> IO [UserRole]
getRoles backend userId =
  flip runSqlPool backend $ do
    userRoleEnts <- selectList [UserRoleStoredUserRoleOwner ==. userId] []
    pure ((\(Entity _ (UserRoleStored x _)) -> x) <$> userRoleEnts)

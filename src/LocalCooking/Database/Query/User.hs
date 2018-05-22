{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , DeriveGeneric
  #-}

module LocalCooking.Database.Query.User where

import LocalCooking.Database.Schema.Facebook.UserDetails
  ( FacebookUserDetails (..), Unique (UniqueFacebookUserId))
import LocalCooking.Database.Schema.Facebook.AccessToken
  ( FacebookUserAccessTokenStored (..), Unique ())
import LocalCooking.Database.Schema.User
  ( User (..), EntityField (UserPassword, UserEmail), UserId, Unique (UniqueEmail)
  )
import LocalCooking.Database.Schema.User.Pending
  ( PendingRegistrationConfirm (..), Unique (UniquePendingRegistration)
  )
import LocalCooking.Database.Schema.User.Role
  ( EntityField (UserRoleStoredUserRoleOwner), UserRoleStored (..)
  )
import LocalCooking.Semantic.Common (Register (..), Login (..), SocialLogin (..), SocialLoginForm (..))
import qualified LocalCooking.Semantic.Common as Semantic
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole (Customer, Admin))
import Facebook.Types (FacebookUserId, FacebookUserAccessToken)

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Text.EmailAddress (EmailAddress)
import Database.Persist (Entity (..), insert, insert_, delete, deleteBy, get, getBy, (=.), update, (==.), selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), oneof)



data RegisterFailure
  = EmailExists
  deriving (Eq, Show, Generic)

instance ToJSON RegisterFailure where
  toJSON x = String $ case x of
    EmailExists -> "email-exists"

instance FromJSON RegisterFailure where
  parseJSON json = case json of
    String x
      | x == "email-exists" -> pure EmailExists
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "RegisterFailure" json

instance Arbitrary RegisterFailure where
  arbitrary = pure EmailExists



registerUser :: ConnectionPool
             -> Register
             -> IO (Either RegisterFailure UserId)
registerUser backend (Register email password social) = do
  now <- getCurrentTime
  flip runSqlPool backend $ do
    mUserId <- liftIO (userIdByEmail backend email)
    case mUserId of
      Just _ -> pure (Left EmailExists)
      Nothing -> do
        userId <- insert (User now email password)
        insert_ (PendingRegistrationConfirm userId)
        liftIO (storeSocialLoginForm backend userId social)
        pure (Right userId)


confirmEmail :: ConnectionPool
             -> EmailAddress
             -> IO Bool
confirmEmail backend email =
  flip runSqlPool backend $ do
    mUserId <- liftIO (userIdByEmail backend email)
    case mUserId of
      Nothing -> pure False
      Just owner -> do
        mPendingEnt <- getBy (UniquePendingRegistration owner)
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
    mEnt <- get owner
    case mEnt of
      Nothing -> pure Nothing
      Just (User _ email _) -> pure (Just email)


userIdByEmail :: ConnectionPool
              -> EmailAddress
              -> IO (Maybe UserId)
userIdByEmail backend email =
  flip runSqlPool backend $ do
    mEmailEnt <- getBy (UniqueEmail email)
    case mEmailEnt of
      Nothing -> pure Nothing
      Just (Entity userId _) -> pure (Just userId)


storeSocialLoginForm :: ConnectionPool
                     -> UserId
                     -> SocialLoginForm
                     -> IO ()
storeSocialLoginForm backend userId (SocialLoginForm mFb) =
  flip runSqlPool backend $
    case mFb of
      Nothing -> pure ()
      Just fbUserId -> insert_ (FacebookUserDetails fbUserId userId)


data LoginFailure
  = BadPassword
  | EmailDoesntExist
  deriving (Eq, Show, Generic)

instance ToJSON LoginFailure where
  toJSON x = String $ case x of
    BadPassword -> "bad-password"
    EmailDoesntExist -> "no-email"

instance FromJSON LoginFailure where
  parseJSON json = case json of
    String x
      | x == "bad-password" -> pure BadPassword
      | x == "no-email" -> pure EmailDoesntExist
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "LoginFailure" json

instance Arbitrary LoginFailure where
  arbitrary = oneof
    [ pure BadPassword
    , pure EmailDoesntExist
    ]


-- | Doesn't write to database, read-only query
login :: ConnectionPool
      -> Login
      -> IO (Either LoginFailure UserId)
login backend (Login email password) =
  flip runSqlPool backend $ do
    mUserEnt <- getBy (UniqueEmail email)
    case mUserEnt of
      Nothing -> pure (Left EmailDoesntExist)
      Just (Entity owner (User _ _ password'))
        | password == password' -> pure (Right owner)
        | otherwise -> pure (Left BadPassword)


-- | NOTE: Doesn't verify the authenticity of FacebookUserAccessToken, but stores it
socialLogin :: ConnectionPool
            -> SocialLogin
            -> IO (Maybe UserId)
socialLogin backend social =
  flip runSqlPool backend $
    case social of
      SocialLoginFB fbUserId fbToken -> do
        mDetails <- getBy (UniqueFacebookUserId fbUserId)
        case mDetails of
          Nothing -> pure Nothing
          Just (Entity fbUserIdId (FacebookUserDetails _ owner)) -> do
            insert_ (FacebookUserAccessTokenStored fbToken fbUserIdId)
            pure (Just owner)


changeSecurityDetails :: ConnectionPool
                      -> UserId
                      -> Semantic.User
                      -> HashedPassword
                      -> IO Bool
changeSecurityDetails backend userId (Semantic.User email newPassword social conf) password =
  flip runSqlPool backend $ do
    mUser <- get userId
    case mUser of
      Nothing -> pure False
      Just (User _ _ password')
        | password' /= password -> pure False
        | otherwise -> do
            update userId [UserEmail =. email, UserPassword =. newPassword]
            liftIO (storeSocialLoginForm backend userId social)
            when conf $
              void $ liftIO $ confirmEmail backend email
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
      Just (User _ _ password') -> pure (password == password')


-- getUsers :: ConnectionPool
--          -> IO [Semantic.User]
-- getUsers backend =
--   flip runSqlPool backend $ do
--     userIds <- selectList [] []
--     fmap catMaybes $ forM userIds $ \(Entity userId _) -> do
--       mEmail <- liftIO (getEmail backend userId)
--       roles <- liftIO (getRoles backend userId)
--       pure $ (,roles) <$> mEmail

-- FIXME Should be defined in admin
-- deleteUser :: ConnectionPool
--            -> EmailAddress
--            -> IO ()
-- deleteUser backend e =
--   flip runSqlPool backend $ do
--     mUid <- liftIO (userIdByEmail backend e)
--     case mUid of
--       Nothing -> pure ()
--       Just uid -> do
--         delete uid
--         deleteBy (EmailAddressOwner uid)
--         deleteBy (UniquePendingRegistration uid)
--         rs <- selectList [UserRoleStoredUserRoleOwner ==. uid] []
--         forM_ rs $ \(Entity key _) -> delete key

-- FIXME Should be defined in admin
-- updateUser :: ConnectionPool
--            -> EmailAddress
--            -> [UserRole]
--            -> Maybe HashedPassword
--            -> IO ()
-- updateUser backend e roles pw =
--   flip runSqlPool backend $ do
--     mUid <- liftIO (userIdByEmail backend e)
--     case mUid of
--       Nothing -> pure ()
--       Just uid -> do
--         let allRoles = Set.fromList [Customer .. Admin]
--             removedRoles = allRoles `Set.difference` Set.fromList roles
--         liftIO $ do
--           forM_ removedRoles (delRole backend uid)
--           forM_ roles (addRole backend uid)
--         case pw of
--           Nothing -> pure ()
--           Just pw' -> update uid [UserPassword =. pw']


-- addRole :: ConnectionPool
--         -> UserId
--         -> UserRole
--         -> IO ()
-- addRole backend userId userRole =
--   flip runSqlPool backend $ do
--     mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
--     case mUserRoleEnt of
--       Just _ -> pure ()
--       Nothing -> insert_ (UserRoleStored userRole userId)


-- delRole :: ConnectionPool
--         -> UserId
--         -> UserRole
--         -> IO ()
-- delRole backend userId userRole =
--   flip runSqlPool backend $ do
--     mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
--     case mUserRoleEnt of
--       Nothing -> pure ()
--       Just (Entity userRoleKey _) -> delete userRoleKey


-- hasRole :: ConnectionPool
--         -> UserId
--         -> UserRole
--         -> IO Bool
-- hasRole backend userId userRole =
--   flip runSqlPool backend $ do
--     mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
--     case mUserRoleEnt of
--       Nothing -> pure False
--       Just _  -> pure True


-- getRoles :: ConnectionPool
--          -> UserId
--          -> IO [UserRole]
-- getRoles backend userId =
--   flip runSqlPool backend $ do
--     userRoleEnts <- selectList [UserRoleStoredUserRoleOwner ==. userId] []
--     pure ((\(Entity _ (UserRoleStored x _)) -> x) <$> userRoleEnts)


-- addPendingEmail :: ConnectionPool
--                 -> UserId
--                 -> IO ()
-- addPendingEmail backend userId =
--   flip runSqlPool backend $
--     insert_ (PendingRegistrationConfirm userId)


-- removePendingEmail :: ConnectionPool
--                    -> UserId
--                    -> IO ()
-- removePendingEmail backend userId =
--   flip runSqlPool backend $ do
--     mEnt <- getBy (UniquePendingRegistration userId)
--     case mEnt of
--       Nothing -> pure ()
--       Just (Entity key _) -> delete key

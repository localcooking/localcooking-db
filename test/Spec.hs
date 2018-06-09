{-# LANGUAGE
    ScopedTypeVariables
  #-}

import LocalCooking.Database.Schema.User
  (EntityField (StoredUserPassword, StoredUserEmail, StoredUserId))
-- import LocalCooking.Database.Schema.User.Pending
--   (EntityField (PendingRegistrationConfirmPendingRegister, PendingRegistrationConfirmId))
import LocalCooking.Database.Schema.User.Role
  (EntityField (UserRoleStoredUserRole, UserRoleStoredUserRoleOwner, UserRoleStoredId))
import LocalCooking.Database.Schema.Facebook.AccessToken (EntityField (FacebookUserAccessTokenStoredFacebookUserAccessToken, FacebookUserAccessTokenStoredFacebookUserDetails, FacebookUserAccessTokenStoredId))
import LocalCooking.Database.Schema.Facebook.UserDetails (EntityField (FacebookUserDetailsFacebookUserId, FacebookUserDetailsFacebookUserOwner, FacebookUserDetailsId))
import LocalCooking.Database.Schema.Salt (EntityField (PasswordSaltPasswordSalt, PasswordSaltId))

import qualified LocalCooking.Semantics.Mitch as Mitch
import qualified LocalCooking.Semantics.Chef as Chef
import qualified LocalCooking.Semantics.Common as Common

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson


main :: IO ()
main = defaultMain $ testGroup "Database Tests"
  [ testGroup "Fields JSON"
    [ testGroup "User.Password"
      [ testCase "UserPassword" $
        assertBool "UserPassword" (jsonIso StoredUserPassword)
      , testCase "UserEmail" $
        assertBool "UserEmail" (jsonIso StoredUserEmail)
      , testCase "UserId" $
        assertBool "UserId" (jsonIso StoredUserId)
      ]
    -- , testGroup "User.Pending"
    --   [ testCase "PendingRegister" $
    --     assertBool "PendingRegister" (jsonIso PendingRegistrationConfirmPendingRegister)
    --   , testCase "PendingRegistrationConfirmId" $
    --     assertBool "PendingRegistrationConfirmId" (jsonIso PendingRegistrationConfirmId)
    --   ]
    , testGroup "User.Role"
      [ testCase "UserRole" $
        assertBool "UserRole" (jsonIso UserRoleStoredUserRole)
      , testCase "UserRoleOwner" $
        assertBool "UserRoleOwner" (jsonIso UserRoleStoredUserRoleOwner)
      , testCase "UserRoleStoredId" $
        assertBool "UserRoleStoredId" (jsonIso UserRoleStoredId)
      ]
    , testGroup "Facebook.AccessToken"
      [ testCase "AccessToken" $
        assertBool "AccessToken" (jsonIso FacebookUserAccessTokenStoredFacebookUserAccessToken)
      , testCase "Details" $
        assertBool "Details" (jsonIso FacebookUserAccessTokenStoredFacebookUserDetails)
      , testCase "FacebookUserAccessTokenStoredId" $
        assertBool "FacebookUserAccessTokenStoredId" (jsonIso FacebookUserAccessTokenStoredId)
      ]
    , testGroup "Facebook.UserDetails"
      [ testCase "FacebookUserId" $
        assertBool "FacebookUserId" (jsonIso FacebookUserDetailsFacebookUserId)
      , testCase "Owner" $
        assertBool "Owner" (jsonIso FacebookUserDetailsFacebookUserOwner)
      , testCase "FacebookUserDetailsStoredId" $
        assertBool "FacebookUserDetailsStoredId" (jsonIso FacebookUserDetailsId)
      ]
    , testGroup "Salt"
      [ testCase "PasswordSalt" $
        assertBool "PasswordSalt" (jsonIso PasswordSaltPasswordSalt)
      , testCase "PasswordSaltId" $
        assertBool "PasswordSaltId" (jsonIso PasswordSaltId)
      ]
    ]
  , testGroup "Semantics JSON"
    [ testProperty "LocalCooking.Semantic.Mitch.ReviewSynopsis"
      (\(x :: Mitch.ReviewSynopsis) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.Review"
      (\(x :: Mitch.Review) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.MealSynopsis"
      (\(x :: Mitch.MealSynopsis) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.Meal"
      (\(x :: Mitch.Meal) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.ChefSynopsis"
      (\(x :: Mitch.ChefSynopsis) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.Chef"
      (\(x :: Mitch.Chef) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.MenuSynopsis"
      (\(x :: Mitch.MenuSynopsis) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.Menu"
      (\(x :: Mitch.Menu) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.Order"
      (\(x :: Mitch.Order) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.Customer"
      (\(x :: Mitch.Customer) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.CartEntry"
      (\(x :: Mitch.CartEntry) -> jsonIso x)

    , testProperty "LocalCooking.Semantic.Chef.MealSettings"
      (\(x :: Chef.MealSettings) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Chef.ChefSettings"
      (\(x :: Chef.ChefSettings) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Chef.MenuSettings"
      (\(x :: Chef.MenuSettings) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Chef.Order"
      (\(x :: Chef.Order) -> jsonIso x)

    , testProperty "LocalCooking.Semantic.Common.SocialLoginForm"
      (\(x :: Common.SocialLoginForm) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.User"
      (\(x :: Common.User) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.Register"
      (\(x :: Common.Register) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.Login"
      (\(x :: Common.Login) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.SocialLogin"
      (\(x :: Common.SocialLogin) -> jsonIso x)
    ]
  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = Just x == Aeson.decode (Aeson.encode x)

{-# LANGUAGE
    ScopedTypeVariables
  #-}

import LocalCooking.Database.Schema
  ( EntityField
    ( StoredUserPassword, StoredUserEmail, StoredUserId
    , UserRoleStoredUserRole, UserRoleStoredUserRoleOwner, UserRoleStoredId
    , FacebookUserAccessTokenStoredFacebookUserAccessToken, FacebookUserAccessTokenStoredFacebookUserDetails, FacebookUserAccessTokenStoredId
    , FacebookUserDetailsFacebookUserId, FacebookUserDetailsFacebookUserOwner, FacebookUserDetailsId
    , PasswordSaltPasswordSalt, PasswordSaltId
    )
  )

import qualified LocalCooking.Semantics.Mitch as Mitch
import qualified LocalCooking.Semantics.Chef as Chef
import qualified LocalCooking.Semantics.Common as Common
import qualified LocalCooking.Semantics.Admin as Admin
import qualified LocalCooking.Semantics.Content as Content
import qualified LocalCooking.Semantics.ContentRecord as ContentRecord

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
    , testProperty "LocalCooking.Semantic.Mitch.GetSetCustomer"
      (\(x :: Mitch.GetSetCustomer) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.Diets"
      (\(x :: Mitch.Diets) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.Allergies"
      (\(x :: Mitch.Allergies) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Mitch.CartEntry"
      (\(x :: Mitch.CartEntry) -> jsonIso x)

    , testProperty "LocalCooking.Semantic.Chef.GetSetChef"
      (\(x :: Chef.GetSetChef) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Chef.MealSettings"
      (\(x :: Chef.MealSettings) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Chef.MenuSettings"
      (\(x :: Chef.MenuSettings) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Chef.Order"
      (\(x :: Chef.Order) -> jsonIso x)

    , testProperty "LocalCooking.Semantic.Common.SocialLoginForm"
      (\(x :: Common.SocialLoginForm) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.User"
      (\(x :: Common.User) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.SetUser"
      (\(x :: Common.SetUser) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.Register"
      (\(x :: Common.Register) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.RegisterError"
      (\(x :: Common.RegisterError) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.ConfirmEmailError"
      (\(x :: Common.ConfirmEmailError) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.Login"
      (\(x :: Common.Login) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.SocialLogin"
      (\(x :: Common.SocialLogin) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Common.WithId"
      (\(x :: Common.WithId () ()) -> jsonIso x)

    , testProperty "LocalCooking.Semantic.Admin.SetUser"
      (\(x :: Admin.SetUser) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Admin.NewUser"
      (\(x :: Admin.NewUser) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Admin.GetSetSubmissionPolicy"
      (\(x :: Admin.GetSetSubmissionPolicy) -> jsonIso x)

    , testProperty "LocalCooking.Semantic.Content.GetEditor"
      (\(x :: Content.GetEditor) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.Content.SetEditor"
      (\(x :: Content.SetEditor) -> jsonIso x)

    , testProperty "LocalCooking.Semantic.ContentRecord.TagRecordVariant"
      (\(x :: ContentRecord.TagRecordVariant) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.ContentRecord.ChefRecordVariant"
      (\(x :: ContentRecord.ChefRecordVariant) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.ContentRecord.ContentRecordVariant"
      (\(x :: ContentRecord.ContentRecordVariant) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.ContentRecord.TagRecord"
      (\(x :: ContentRecord.TagRecord) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.ContentRecord.ChefRecord"
      (\(x :: ContentRecord.ChefRecord) -> jsonIso x)
    , testProperty "LocalCooking.Semantic.ContentRecord.ContentRecord"
      (\(x :: ContentRecord.ContentRecord) -> jsonIso x)
    ]
  , testGroup "Semantics Enum"
    [ testProperty "LocalCooking.Semantic.ContentRecord.TagRecordVariant"
      (\(x :: ContentRecord.TagRecordVariant) -> enumIso x)
    , testProperty "LocalCooking.Semantic.ContentRecord.ChefRecordVariant"
      (\(x :: ContentRecord.ChefRecordVariant) -> enumIso x)
    , testProperty "LocalCooking.Semantic.ContentRecord.ContentRecordVariant"
      (\(x :: ContentRecord.ContentRecordVariant) -> enumIso x)
    ]
  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = Just x == Aeson.decode (Aeson.encode x)


enumIso :: Enum a => Eq a => a -> Bool
enumIso x = x == toEnum (fromEnum x)

import LocalCooking.Database.Schema.User.Password (EntityField (UserPassword, UserId))
import LocalCooking.Database.Schema.User.Email (EntityField (EmailAddressStoredEmailAddress, EmailAddressStoredEmailAddressOwner, EmailAddressStoredId))
import LocalCooking.Database.Schema.User.Pending (EntityField (PendingRegistrationConfirmPendingRegister, PendingRegistrationConfirmId))
import LocalCooking.Database.Schema.User.Role (EntityField (UserRoleStoredUserRole, UserRoleStoredUserRoleOwner, UserRoleStoredId))
import LocalCooking.Database.Schema.Facebook.AccessToken (EntityField (FacebookUserAccessTokenStoredFacebookUserAccessToken, FacebookUserAccessTokenStoredFacebookUserDetails, FacebookUserAccessTokenStoredId))
import LocalCooking.Database.Schema.Facebook.UserDetails (EntityField (FacebookUserDetailsFacebookUserId, FacebookUserDetailsFacebookUserOwner, FacebookUserDetailsId))
import LocalCooking.Database.Schema.Salt (EntityField (PasswordSaltPasswordSalt, PasswordSaltId))

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson


main :: IO ()
main = defaultMain $ testGroup "Database Tests"
  [ testGroup "Fields Json"
    [ testGroup "User.Password"
      [ testCase "UserPassword" $
        assertBool "UserPassword" (jsonIso UserPassword)
      , testCase "UserId" $
        assertBool "UserId" (jsonIso UserId)
      ]
    , testGroup "User.Email"
      [ testCase "EmailAddress" $
        assertBool "EmailAddress" (jsonIso EmailAddressStoredEmailAddress)
      , testCase "EmailAddressOwner" $
        assertBool "EmailAddressOwner" (jsonIso EmailAddressStoredEmailAddressOwner)
      , testCase "EmailAddressStoredId" $
        assertBool "EmailAddressStoredId" (jsonIso EmailAddressStoredId)
      ]
    , testGroup "User.Pending"
      [ testCase "PendingRegister" $
        assertBool "PendingRegister" (jsonIso PendingRegistrationConfirmPendingRegister)
      , testCase "PendingRegistrationConfirmId" $
        assertBool "PendingRegistrationConfirmId" (jsonIso PendingRegistrationConfirmId)
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
  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = Just x == Aeson.decode (Aeson.encode x)

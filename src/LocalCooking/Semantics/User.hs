{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.User where

{-

First-person person perspective via AuthToken

-}

import LocalCooking.Common.User.Role (UserRole)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Traversable (traverse)
import Data.Foldable (asum)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()


data UserExists a
  = UserDoesntExist
  | UserExists a
  deriving (Eq, Show, Generic)

instance Arbitrary a => Arbitrary (UserExists a) where
  arbitrary = oneof
    [ pure UserDoesntExist
    , UserExists <$> arbitrary
    ]

instance ToJSON a => ToJSON (UserExists a) where
  toJSON x = case x of
    UserDoesntExist -> String "userDoesntExist"
    UserExists a -> object ["userExists" .= a]

instance FromJSON a => FromJSON (UserExists a) where
  parseJSON x = case x of
    String s
      | s == "userDoesntExist" -> pure UserDoesntExist
      | otherwise -> typeMismatch "UserExists" x
    Object o -> UserExists <$> o .: "userExists"


data HasRole a
  = DoesntHaveRole
  | HasRole a
  deriving (Eq, Show, Generic)

instance Arbitrary a => Arbitrary (HasRole a) where
  arbitrary = oneof
    [ pure DoesntHaveRole
    , HasRole <$> arbitrary
    ]

instance ToJSON a => ToJSON (HasRole a) where
  toJSON x = case x of
    DoesntHaveRole -> String "doesntHaveRole"
    HasRole a -> object ["hasRole" .= a]

instance FromJSON a => FromJSON (HasRole a) where
  parseJSON x = case x of
    String s
      | s == "doesntHaveRole" -> pure DoesntHaveRole
      | otherwise -> typeMismatch "HasRole" x
    Object o -> HasRole <$> o .: "hasRole"

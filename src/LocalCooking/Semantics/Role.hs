{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.Role where

import LocalCooking.Common.User.Role (UserRole)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, Null), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Traversable (traverse)
import Data.Foldable (asum)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()



data HasRole a
  = DoesntHaveRole
  | HasRole UserRole a
  deriving (Eq, Show, Generic)

instance Arbitrary a => Arbitrary (HasRole a) where
  arbitrary = oneof
    [ pure DoesntHaveRole
    , HasRole <$> arbitrary <*> arbitrary
    ]

instance ToJSON a => ToJSON (HasRole a) where
  toJSON x = case x of
    DoesntHaveRole -> Null
    HasRole role a -> object [T.pack (show role) .= a]

instance FromJSON a => FromJSON (HasRole a) where
  parseJSON x = case x of
    Null -> pure DoesntHaveRole
    Object o -> do
      let getRole role = HasRole role <$> o .: T.pack (show role)
      asum (map getRole [minBound .. maxBound])

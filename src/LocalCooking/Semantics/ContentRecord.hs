{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.ContentRecord where

import LocalCooking.Semantics.ContentRecord.Variant
  ( ContentRecordVariant (..), TagRecordVariant (..), ChefRecordVariant (..), ProfileRecordVariant (..))
import LocalCooking.Semantics.Common (WithId)
import LocalCooking.Semantics.Chef (SetChef, MealSettings, MenuSettings)
import LocalCooking.Semantics.Mitch (SetCustomer)
import LocalCooking.Semantics.Content (SetEditor)
import LocalCooking.Database.Schema (StoredMenuId, StoredMealId)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Culture (CultureTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Meal (MealTag)

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Control.Applicative ((<|>))
import Database.Persist.TH (derivePersistFieldJSON)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()



-- * Records


data TagRecord
  = TagRecordChef       ChefTag
  | TagRecordCulture    CultureTag
  | TagRecordDiet       DietTag
  | TagRecordFarm       FarmTag
  | TagRecordIngredient IngredientTag
  | TagRecordMeal       MealTag
  deriving (Eq, Show, Generic)
derivePersistFieldJSON "TagRecord"

instance Arbitrary TagRecord where
  arbitrary = oneof
    [ TagRecordChef <$> arbitrary
    , TagRecordCulture <$> arbitrary
    , TagRecordDiet <$> arbitrary
    , TagRecordFarm <$> arbitrary
    , TagRecordIngredient <$> arbitrary
    , TagRecordMeal <$> arbitrary
    ]

instance ToJSON TagRecord where
  toJSON x = case x of
    TagRecordChef y -> object ["chef" .= y]
    TagRecordCulture y -> object ["culture" .= y]
    TagRecordDiet y -> object ["diet" .= y]
    TagRecordFarm y -> object ["farm" .= y]
    TagRecordIngredient y -> object ["ingredient" .= y]
    TagRecordMeal y -> object ["meal" .= y]

instance FromJSON TagRecord where
  parseJSON json = case json of
    Object o -> do
      let chef = TagRecordChef <$> o .: "chef"
          culture = TagRecordCulture <$> o .: "culture"
          diet = TagRecordDiet <$> o .: "diet"
          farm = TagRecordFarm <$> o .: "farm"
          ingredient = TagRecordIngredient <$> o .: "ingredient"
          meal = TagRecordMeal <$> o .: "meal"
      chef <|> culture <|> diet <|> farm <|> ingredient <|> meal
    _ -> typeMismatch "TagRecord" json

tagRecordVariant :: TagRecord -> TagRecordVariant
tagRecordVariant x = case x of
  TagRecordChef _ -> TagVariantChef
  TagRecordCulture _ -> TagVariantCulture
  TagRecordDiet _ -> TagVariantDiet
  TagRecordFarm _ -> TagVariantFarm
  TagRecordIngredient _ -> TagVariantIngredient
  TagRecordMeal _ -> TagVariantMeal



data ChefRecord
  = ChefRecordSetMenu (WithId StoredMenuId MenuSettings)
  | ChefRecordNewMenu MenuSettings
  | ChefRecordSetMeal (WithId StoredMenuId (WithId StoredMealId MealSettings))
  | ChefRecordNewMeal (WithId StoredMenuId MealSettings)
  deriving (Eq, Show, Generic)
derivePersistFieldJSON "ChefRecord"

instance Arbitrary ChefRecord where
  arbitrary = oneof
    [ ChefRecordSetMenu <$> arbitrary
    , ChefRecordNewMenu <$> arbitrary
    , ChefRecordSetMeal <$> arbitrary
    , ChefRecordNewMeal <$> arbitrary
    ]

instance ToJSON ChefRecord where
  toJSON x = case x of
    ChefRecordSetMenu y -> object ["setMenu" .= y]
    ChefRecordNewMenu y -> object ["newMenu" .= y]
    ChefRecordSetMeal y -> object ["setMenu" .= y]
    ChefRecordNewMeal y -> object ["newMenu" .= y]

instance FromJSON ChefRecord where
  parseJSON json = case json of
    Object o -> do
      let setMenu = ChefRecordSetMenu <$> o .: "setMenu"
          newMenu = ChefRecordNewMenu <$> o .: "newMenu"
          setMeal = ChefRecordSetMeal <$> o .: "setMenu"
          newMeal = ChefRecordNewMeal <$> o .: "newMenu"
      setMenu <|> newMenu <|> setMeal <|> newMeal
    _ -> typeMismatch "ChefRecord" json

chefRecordVariant :: ChefRecord -> ChefRecordVariant
chefRecordVariant x = case x of
  ChefRecordSetMenu _ -> ChefVariantMenu
  ChefRecordNewMenu _ -> ChefVariantMenu
  ChefRecordSetMeal _ -> ChefVariantMeal
  ChefRecordNewMeal _ -> ChefVariantMeal



data ProfileRecord
  = ProfileRecordChef SetChef
  | ProfileRecordCustomer SetCustomer
  | ProfileRecordEditor SetEditor
  -- TODO farmer restaurant
  deriving (Eq, Show, Generic)
derivePersistFieldJSON "ProfileRecord"

instance Arbitrary ProfileRecord where
  arbitrary = oneof
    [ ProfileRecordChef <$> arbitrary
    , ProfileRecordCustomer <$> arbitrary
    , ProfileRecordEditor <$> arbitrary
    ]

instance ToJSON ProfileRecord where
  toJSON x = case x of
    ProfileRecordChef y -> object ["chef" .= y]
    ProfileRecordCustomer y -> object ["customer" .= y]
    ProfileRecordEditor y -> object ["editor" .= y]

instance FromJSON ProfileRecord where
  parseJSON json = case json of
    Object o -> do
      let chef = ProfileRecordChef <$> o .: "chef"
          customer = ProfileRecordCustomer <$> o .: "customer"
          editor = ProfileRecordEditor <$> o .: "editor"
      chef <|> customer <|> editor
    _ -> typeMismatch "ProfileRecord" json

profileRecordVariant :: ProfileRecord -> ProfileRecordVariant
profileRecordVariant x = case x of
  ProfileRecordChef _ -> ProfileVariantChef
  ProfileRecordCustomer _ -> ProfileVariantCustomer
  ProfileRecordEditor _ -> ProfileVariantEditor



data ContentRecord
  = TagRecord TagRecord
  | ChefRecord ChefRecord
  | ProfileRecord ProfileRecord
  deriving (Eq, Show, Generic)
derivePersistFieldJSON "ContentRecord"

instance Arbitrary ContentRecord where
  arbitrary = oneof
    [ TagRecord <$> arbitrary
    , ChefRecord <$> arbitrary
    , ProfileRecord <$> arbitrary
    ]

instance ToJSON ContentRecord where
  toJSON x = case x of
    TagRecord y -> object ["tagRecord" .= y]
    ChefRecord y -> object ["chefRecord" .= y]
    ProfileRecord y -> object ["profileRecord" .= y]

instance FromJSON ContentRecord where
  parseJSON json = case json of
    Object o -> do
      let tag = TagRecord <$> o .: "tagRecord"
          chef = ChefRecord <$> o .: "chefRecord"
          profile = ProfileRecord <$> o .: "profileRecord"
      tag <|> chef <|> profile
    _ -> typeMismatch "ContentRecord" json


contentRecordVariant :: ContentRecord -> ContentRecordVariant
contentRecordVariant x = case x of
  TagRecord y -> TagRecordVariant (tagRecordVariant y)
  ChefRecord y -> ChefRecordVariant (chefRecordVariant y)
  ProfileRecord y -> ProfileRecordVariant (profileRecordVariant y)

{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.ContentRecord where

import LocalCooking.Database.Schema (StoredEditorId)
import LocalCooking.Common.User.Name (Name)
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



-- * Variants

data TagRecordVariant
  = TagVariantChef
  | TagVariantCulture
  | TagVariantDiet
  | TagVariantFarm
  | TagVariantIngredient
  | TagVariantMeal
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)
derivePersistFieldJSON "TagRecordVariant"

instance Arbitrary TagRecordVariant where
  arbitrary = oneof
    [ pure TagVariantChef
    , pure TagVariantCulture
    , pure TagVariantDiet
    , pure TagVariantFarm
    , pure TagVariantIngredient
    , pure TagVariantMeal
    ]

instance Hashable TagRecordVariant

instance ToJSON TagRecordVariant where
  toJSON x = String $ case x of
    TagVariantChef -> "chefTag"
    TagVariantCulture -> "cultureTag"
    TagVariantDiet -> "dietTag"
    TagVariantFarm -> "farmTag"
    TagVariantIngredient -> "ingredientTag"
    TagVariantMeal -> "mealTag"

instance FromJSON TagRecordVariant where
  parseJSON = attoAeson tagRecordVariantParser

tagRecordVariantParser :: Parser TagRecordVariant
tagRecordVariantParser = do
  let chef = TagVariantChef <$ string "chefTag"
      culture = TagVariantCulture <$ string "cultureTag"
      diet = TagVariantDiet <$ string "dietTag"
      farm = TagVariantFarm <$ string "farmTag"
      ingredient = TagVariantIngredient <$ string "ingredientTag"
      meal = TagVariantMeal <$ string "mealTag"
  chef <|> culture <|> diet <|> farm <|> ingredient <|> meal





-- | Top-level nullary storable variant declaring which type of content is stored
data ContentRecordVariant
  = TagRecordVariant TagRecordVariant
  deriving (Eq, Ord, Show, Read, Generic)
derivePersistFieldJSON "ContentRecordVariant"

instance Enum ContentRecordVariant where
  fromEnum x = case x of
    TagRecordVariant y -> adjustTag (fromEnum y)
    where
      -- inclusive bounds for each type
      minBoundTag = fromEnum (minBound :: TagRecordVariant)
      maxBoundTag = fromEnum (maxBound :: TagRecordVariant)
      adjustTag j = j + 0 -- maxBoundTag + 1
  toEnum i
    |  i < 0 = minBound
    |  i >= minBoundTag
    && i <= maxBoundTag = TagRecordVariant (toEnum (adjustTag i))
    |  otherwise = maxBound
    where
      -- inclusive bounds for each type
      minBoundTag = fromEnum (minBound :: TagRecordVariant)
      maxBoundTag = fromEnum (maxBound :: TagRecordVariant)
      adjustTag j = j - 0 -- maxBoundTag + 1

instance Bounded ContentRecordVariant where
  minBound = TagRecordVariant TagVariantChef
  maxBound = TagRecordVariant TagVariantMeal

instance Arbitrary ContentRecordVariant where
  arbitrary = oneof
    [ TagRecordVariant <$> arbitrary
    ]

instance Hashable ContentRecordVariant

instance ToJSON ContentRecordVariant where
  toJSON x = case x of
    TagRecordVariant y -> object ["tagVariant" .= y]

instance FromJSON ContentRecordVariant where
  parseJSON json = case json of
    Object o -> do
      let tag = TagRecordVariant <$> o .: "tagVariant"
      tag
    _ -> typeMismatch "ContentRecordVariant" json



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



data ContentRecord
  = TagRecord TagRecord
  deriving (Eq, Show, Generic)
derivePersistFieldJSON "ContentRecord"

instance Arbitrary ContentRecord where
  arbitrary = oneof
    [ TagRecord <$> arbitrary
    ]

instance ToJSON ContentRecord where
  toJSON x = case x of
    TagRecord y -> object ["tagRecord" .= y]

instance FromJSON ContentRecord where
  parseJSON json = case json of
    Object o -> do
      let tag = TagRecord <$> o .: "tagRecord"
      tag
    _ -> typeMismatch "ContentRecord" json


contentRecordVariant :: ContentRecord -> ContentRecordVariant
contentRecordVariant x = case x of
  TagRecord y -> TagRecordVariant (tagRecordVariant y)

{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  #-}

module LocalCooking.Semantics.ContentRecord where

import LocalCooking.Semantics.Common (WithId)
import LocalCooking.Semantics.Chef (GetSetChef, MealSettings, MenuSettings)
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



data ChefRecordVariant
  = ChefVariantChef
  | ChefVariantMenu
  | ChefVariantMeal
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)
derivePersistFieldJSON "ChefRecordVariant"

instance Arbitrary ChefRecordVariant where
  arbitrary = oneof
    [ pure ChefVariantChef
    , pure ChefVariantMenu
    , pure ChefVariantMeal
    ]

instance Hashable ChefRecordVariant

instance ToJSON ChefRecordVariant where
  toJSON x = String $ case x of
    ChefVariantChef -> "chefChef"
    ChefVariantMenu -> "menuChef"
    ChefVariantMeal -> "mealChef"

instance FromJSON ChefRecordVariant where
  parseJSON = attoAeson chefRecordVariantParser

chefRecordVariantParser :: Parser ChefRecordVariant
chefRecordVariantParser = do
  let chef = ChefVariantChef <$ string "chefChef"
      menu = ChefVariantMenu <$ string "menuChef"
      meal = ChefVariantMeal <$ string "mealChef"
  chef <|> menu <|> meal





-- | Top-level nullary storable variant declaring which type of content is stored
data ContentRecordVariant
  = TagRecordVariant TagRecordVariant
  | ChefRecordVariant ChefRecordVariant
  deriving (Eq, Ord, Show, Read, Generic)
derivePersistFieldJSON "ContentRecordVariant"

instance Enum ContentRecordVariant where
  fromEnum x = case x of
    TagRecordVariant y -> adjustTag (fromEnum y)
    ChefRecordVariant y -> adjustChef (fromEnum y)
    where
      -- inclusive bounds for each type
      minBoundTag = fromEnum (minBound :: TagRecordVariant)
      maxBoundTag = fromEnum (maxBound :: TagRecordVariant)
      minBoundChef = fromEnum (minBound :: ChefRecordVariant)
      maxBoundChef = fromEnum (maxBound :: ChefRecordVariant)
      adjustTag j = j + 0 -- maxBoundTag + 1
      adjustChef j = j + maxBoundTag + 1
  toEnum i
    |  i < 0 = minBound
    |  i >= minBoundTag
    && i <= maxBoundTag = TagRecordVariant (toEnum (adjustTag i))
    |  i >= minBoundChef
    && i <= maxBoundChef = ChefRecordVariant (toEnum (adjustChef i))
    |  otherwise = maxBound
    where
      -- inclusive bounds for each type
      minBoundTag = fromEnum (minBound :: TagRecordVariant)
      maxBoundTag = fromEnum (maxBound :: TagRecordVariant)
      minBoundChef = fromEnum (minBound :: ChefRecordVariant) + maxBoundTag + 1
      maxBoundChef = fromEnum (maxBound :: ChefRecordVariant) + maxBoundTag + 1
      adjustTag j = j - 0 -- maxBoundTag + 1
      adjustChef j = j - (maxBoundTag + 1)

instance Bounded ContentRecordVariant where
  minBound = TagRecordVariant TagVariantChef
  maxBound = ChefRecordVariant ChefVariantMenu

instance Arbitrary ContentRecordVariant where
  arbitrary = oneof
    [ TagRecordVariant <$> arbitrary
    , ChefRecordVariant <$> arbitrary
    ]

instance Hashable ContentRecordVariant

instance ToJSON ContentRecordVariant where
  toJSON x = case x of
    TagRecordVariant y -> object ["tagVariant" .= y]
    ChefRecordVariant y -> object ["chefVariant" .= y]

instance FromJSON ContentRecordVariant where
  parseJSON json = case json of
    Object o -> do
      let tag = TagRecordVariant <$> o .: "tagVariant"
          chef = ChefRecordVariant <$> o .: "chefVariant"
      tag <|> chef
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



data ChefRecord
  = ChefRecordChef GetSetChef
  | ChefRecordSetMenu (WithId StoredMenuId MenuSettings)
  | ChefRecordNewMenu MenuSettings
  | ChefRecordSetMeal (WithId StoredMenuId (WithId StoredMealId MealSettings))
  | ChefRecordNewMeal (WithId StoredMenuId MealSettings)
  deriving (Eq, Show, Generic)
derivePersistFieldJSON "ChefRecord"

instance Arbitrary ChefRecord where
  arbitrary = oneof
    [ ChefRecordChef <$> arbitrary
    , ChefRecordSetMenu <$> arbitrary
    , ChefRecordNewMenu <$> arbitrary
    , ChefRecordSetMeal <$> arbitrary
    , ChefRecordNewMeal <$> arbitrary
    ]

instance ToJSON ChefRecord where
  toJSON x = case x of
    ChefRecordChef y -> object ["chef" .= y]
    ChefRecordSetMenu y -> object ["setMenu" .= y]
    ChefRecordNewMenu y -> object ["newMenu" .= y]
    ChefRecordSetMeal y -> object ["setMenu" .= y]
    ChefRecordNewMeal y -> object ["newMenu" .= y]

instance FromJSON ChefRecord where
  parseJSON json = case json of
    Object o -> do
      let chef = ChefRecordChef <$> o .: "chef"
          setMenu = ChefRecordSetMenu <$> o .: "setMenu"
          newMenu = ChefRecordNewMenu <$> o .: "newMenu"
          setMeal = ChefRecordSetMeal <$> o .: "setMenu"
          newMeal = ChefRecordNewMeal <$> o .: "newMenu"
      chef <|> setMenu <|> newMenu <|> setMeal <|> newMeal
    _ -> typeMismatch "ChefRecord" json

chefRecordVariant :: ChefRecord -> ChefRecordVariant
chefRecordVariant x = case x of
  ChefRecordChef _ -> ChefVariantChef
  ChefRecordSetMenu _ -> ChefVariantMenu
  ChefRecordNewMenu _ -> ChefVariantMenu
  ChefRecordSetMeal _ -> ChefVariantMeal
  ChefRecordNewMeal _ -> ChefVariantMeal



data ContentRecord
  = TagRecord TagRecord
  | ChefRecord ChefRecord
  deriving (Eq, Show, Generic)
derivePersistFieldJSON "ContentRecord"

instance Arbitrary ContentRecord where
  arbitrary = oneof
    [ TagRecord <$> arbitrary
    , ChefRecord <$> arbitrary
    ]

instance ToJSON ContentRecord where
  toJSON x = case x of
    TagRecord y -> object ["tagRecord" .= y]
    ChefRecord y -> object ["chefRecord" .= y]

instance FromJSON ContentRecord where
  parseJSON json = case json of
    Object o -> do
      let tag = TagRecord <$> o .: "tagRecord"
          chef = ChefRecord <$> o .: "chefRecord"
      tag <|> chef
    _ -> typeMismatch "ContentRecord" json


contentRecordVariant :: ContentRecord -> ContentRecordVariant
contentRecordVariant x = case x of
  TagRecord y -> TagRecordVariant (tagRecordVariant y)
  ChefRecord y -> ChefRecordVariant (chefRecordVariant y)

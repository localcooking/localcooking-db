{-# LANGUAGE
    DeriveGeneric
  , TemplateHaskell
  , OverloadedStrings
  #-}

module LocalCooking.Semantics.ContentRecord.Variant where

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object, String), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Database.Persist.TH (derivePersistFieldJSON)
import Data.Hashable (Hashable)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)

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
  = ChefVariantMenu
  | ChefVariantMeal
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)
derivePersistFieldJSON "ChefRecordVariant"

instance Arbitrary ChefRecordVariant where
  arbitrary = oneof
    [ pure ChefVariantMenu
    , pure ChefVariantMeal
    ]

instance Hashable ChefRecordVariant

instance ToJSON ChefRecordVariant where
  toJSON x = String $ case x of
    ChefVariantMenu -> "menuChef"
    ChefVariantMeal -> "mealChef"

instance FromJSON ChefRecordVariant where
  parseJSON = attoAeson chefRecordVariantParser

chefRecordVariantParser :: Parser ChefRecordVariant
chefRecordVariantParser = do
  let menu = ChefVariantMenu <$ string "menuChef"
      meal = ChefVariantMeal <$ string "mealChef"
  menu <|> meal



data ProfileRecordVariant
  = ProfileVariantChef
  | ProfileVariantCustomer
  | ProfileVariantEditor
  | ProfileVariantFarmer
  | ProfileVariantRestaurant
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)
derivePersistFieldJSON "ProfileRecordVariant"

instance Arbitrary ProfileRecordVariant where
  arbitrary = oneof
    [ pure ProfileVariantChef
    , pure ProfileVariantCustomer
    , pure ProfileVariantEditor
    , pure ProfileVariantFarmer
    , pure ProfileVariantRestaurant
    ]

instance Hashable ProfileRecordVariant

instance ToJSON ProfileRecordVariant where
  toJSON x = String $ case x of
    ProfileVariantChef -> "chefProfile"
    ProfileVariantCustomer -> "customerProfile"
    ProfileVariantEditor -> "editorProfile"
    ProfileVariantFarmer -> "farmerProfile"
    ProfileVariantRestaurant -> "restaurantProfile"

instance FromJSON ProfileRecordVariant where
  parseJSON = attoAeson profileRecordVariantParser

profileRecordVariantParser :: Parser ProfileRecordVariant
profileRecordVariantParser = do
  let chef = ProfileVariantChef <$ string "chefProfile"
      customer = ProfileVariantCustomer <$ string "customerProfile"
      editor = ProfileVariantEditor <$ string "editorProfile"
      farmer = ProfileVariantFarmer <$ string "farmerProfile"
      restaurant = ProfileVariantRestaurant <$ string "restaurantProfile"
  chef <|> customer <|> editor <|> farmer <|> restaurant





-- | Top-level nullary storable variant declaring which type of content is stored
data ContentRecordVariant
  = TagRecordVariant TagRecordVariant
  | ChefRecordVariant ChefRecordVariant
  | ProfileRecordVariant ProfileRecordVariant
  deriving (Eq, Ord, Show, Read, Generic)
derivePersistFieldJSON "ContentRecordVariant"

instance Enum ContentRecordVariant where
  fromEnum x = case x of
    TagRecordVariant y -> adjustTag (fromEnum y)
    ChefRecordVariant y -> adjustChef (fromEnum y)
    ProfileRecordVariant y -> adjustProfile (fromEnum y)
    where
      -- inclusive bounds for each type
      minBoundTag = fromEnum (minBound :: TagRecordVariant)
      maxBoundTag = fromEnum (maxBound :: TagRecordVariant)
      minBoundChef = fromEnum (minBound :: ChefRecordVariant)
      maxBoundChef = fromEnum (maxBound :: ChefRecordVariant)
      minBoundProfile = fromEnum (minBound :: ProfileRecordVariant)
      maxBoundProfile = fromEnum (maxBound :: ProfileRecordVariant)
      adjustTag j = j + 0 -- maxBoundTag + 1
      adjustChef j = adjustTag (j + maxBoundTag + 1)
      adjustProfile j = adjustChef (j + maxBoundChef + 1)

  toEnum i
    |  i < 0 = minBound
    |  i >= minBoundTag
    && i <= maxBoundTag = TagRecordVariant (toEnum (adjustTag i))
    |  i >= minBoundChef
    && i <= maxBoundChef = ChefRecordVariant (toEnum (adjustChef i))
    |  i >= minBoundProfile
    && i <= maxBoundProfile = ProfileRecordVariant (toEnum (adjustProfile i))
    |  otherwise = maxBound
    where
      -- inclusive bounds for each type
      minBoundTag = fromEnum (minBound :: TagRecordVariant)
      maxBoundTag = fromEnum (maxBound :: TagRecordVariant)
      minBoundChef = fromEnum (minBound :: ChefRecordVariant) + maxBoundTag + 1
      maxBoundChef = fromEnum (maxBound :: ChefRecordVariant) + maxBoundTag + 1
      minBoundProfile = fromEnum (minBound :: ProfileRecordVariant) + maxBoundChef + 1
      maxBoundProfile = fromEnum (maxBound :: ProfileRecordVariant) + maxBoundChef + 1
      adjustTag j = j - 0 -- maxBoundTag + 1
      adjustChef j = j - (maxBoundTag + 1)
      adjustProfile j = j - (maxBoundChef + 1)

instance Bounded ContentRecordVariant where
  minBound = TagRecordVariant TagVariantChef
  maxBound = ProfileRecordVariant ProfileVariantRestaurant

instance Arbitrary ContentRecordVariant where
  arbitrary = oneof
    [ TagRecordVariant <$> arbitrary
    , ChefRecordVariant <$> arbitrary
    , ProfileRecordVariant <$> arbitrary
    ]

instance Hashable ContentRecordVariant

instance ToJSON ContentRecordVariant where
  toJSON x = case x of
    TagRecordVariant y -> object ["tagVariant" .= y]
    ChefRecordVariant y -> object ["chefVariant" .= y]
    ProfileRecordVariant y -> object ["profileVariant" .= y]

instance FromJSON ContentRecordVariant where
  parseJSON json = case json of
    Object o -> do
      let tag = TagRecordVariant <$> o .: "tagVariant"
          chef = ChefRecordVariant <$> o .: "chefVariant"
          profile = ProfileRecordVariant <$> o .: "profileVariant"
      tag <|> chef <|> profile
    _ -> typeMismatch "ContentRecordVariant" json



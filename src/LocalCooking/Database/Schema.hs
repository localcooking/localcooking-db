{-# LANGUAGE
    QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , MultiParamTypeClasses
  , ExistentialQuantification
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema where

import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.User.Password (HashedPassword (..))
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Culture (CultureTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Ingredient (Ingredient (..))
import Facebook.Types (FacebookUserId, FacebookUserAccessToken)

import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Markdown (MarkdownText)
import Data.Text.Permalink (Permalink)
import Data.Price (Price)
import Data.Address (USAAddress)
import Data.Hashable (Hashable (..))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), encode)
import Data.Aeson.Types (typeMismatch)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Text.EmailAddress (EmailAddress)
import Control.Monad (forM_, forM, void)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.IO.Class (liftIO)
import Database.Persist (Entity (..), (=.), (==.))
import Database.Persist.Class
  ( PersistEntity (Key, EntityField)
  , insert_, update, delete, deleteWhere, get, getBy, selectFirst, selectList
  )
import Database.Persist.Sql (SqlBackend, toSqlKey)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Test.QuickCheck (Arbitrary (..))
import Unsafe.Coerce (unsafeCoerce)
import Crypto.Saltine.Core.Utils (randomByteString)



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

-- * User

StoredUser
    created UTCTime
    email EmailAddress
    password HashedPassword
    confirmed Bool
    UniqueEmail email
    deriving Eq Show

FacebookUserAccessTokenStored
    facebookUserAccessToken FacebookUserAccessToken
    facebookUserDetails FacebookUserDetailsId
    UniqueFacebookUserAccessToken facebookUserAccessToken
    FacebookUserAccessTokenOwner facebookUserDetails
    deriving Eq Show

FacebookUserDetails
    facebookUserId FacebookUserId
    facebookUserOwner StoredUserId
    UniqueFacebookUserId facebookUserId
    FacebookUserDetailsOwner facebookUserOwner
    deriving Eq Show

UserRoleStored
    userRole UserRole
    userRoleOwner StoredUserId
    UniqueUserRole userRole userRoleOwner
    deriving Eq Show

-- ** Customer User

StoredCustomer
    storedCustomerOwner StoredUserId
    storedCustomerName Name
    storedCustomerAddress USAAddress
    UniqueCustomer storedCustomerOwner
    deriving Eq Show

StoredDietPreference
    dietPreferenceOwner StoredCustomerId
    dietPreferenceDiet  StoredDietTagId
    UniqueDietPreference dietPreferenceOwner dietPreferenceDiet
    deriving Eq Show

StoredAllergy
    allergyOwner StoredCustomerId
    allergy      StoredIngredientTagId
    UniqueAllergy allergyOwner allergy
    deriving Eq Show

-- ** Chef User

StoredChef
    storedChefOwner StoredUserId
    storedChefName Name
    storedChefPermalink Permalink
    storedChefImages [ImageSource]
    storedChefAvatar ImageSource
    storedChefBio MarkdownText
    UniqueChefOwner storedChefOwner
    UniqueChefPermalink storedChefPermalink
    deriving Eq Show

ChefTagRelation
    chefTagChef StoredChefId
    chefTagChefTag StoredChefTagId
    UniqueChefTag chefTagChef chefTagChefTag
    deriving Eq Show

-- ** Farmer User

StoredFarmer
    storedFarmerOwner StoredUserId
    storedFarmerName Name
    storedFarmerPermalink Permalink
    storedFarmerImages [ImageSource]
    storedFarmerAvatar ImageSource
    storedFarmerBio MarkdownText
    UniqueFarmerOwner storedFarmerOwner
    UniqueFarmerPermalink storedFarmerPermalink
    deriving Eq Show

FarmTagRelation
    farmTagFarmer StoredFarmerId
    farmTagFarmTag StoredFarmTagId
    UniqueFarmTag farmTagFarmer farmTagFarmTag
    deriving Eq Show

-- ** Editor User

StoredEditor
    storedEditorOwner StoredUserId
    storedEditorName Name
    UniqueEditor storedEditorOwner
    deriving Eq Show


-- * Meal

StoredMeal
    storedMealTitle Text
    storedMealPermalink Permalink
    storedMealMenu StoredMenuId
    storedMealHeading Text
    storedMealDescription MarkdownText
    storedMealInstructions MarkdownText
    storedMealImages [ImageSource]
    storedMealPrice Price
    UniqueMealPermalink storedMealMenu storedMealPermalink
    deriving Eq Show

MealIngredient
    mealIngredientMeal StoredMealId
    mealIngredientIngredient StoredIngredientTagId
    UniqueMealIngredient mealIngredientMeal mealIngredientIngredient
    deriving Eq Show

MealTagRelation
    mealTagMeal StoredMealId
    mealTagMealTag StoredMealTagId
    UniqueMealTag mealTagMeal mealTagMealTag
    deriving Eq Show


-- * Menu

StoredMenu
    storedMenuPublished Day Maybe
    storedMenuDeadline Day
    storedMenuHeading Text
    storedMenuDescription MarkdownText
    storedMenuImages [ImageSource]
    storedMenuAuthor StoredChefId
    UniqueMenuDeadline storedMenuAuthor storedMenuDeadline
    deriving Eq Show

MenuTagRelation
    menuTagMenu StoredMenuId
    menuTagMealTag StoredMealTagId
    UniqueMenuTag menuTagMenu menuTagMealTag
    deriving Eq Show


-- * Diets

IngredientDietViolation
    ingredientViolator StoredIngredientTagId
    dietViolated StoredDietTagId
    UniqueViolation ingredientViolator dietViolated
    deriving Eq Show


-- * Review

StoredReview
    storedReviewOrder StoredOrderId
    storedReviewChef StoredChefId
    storedReviewMeal StoredMealId
    storedReviewAuthor StoredCustomerId
    storedReviewRating Rating
    storedReviewSubmitted UTCTime
    storedReviewHeading Text
    storedReviewBody MarkdownText
    storedReviewImages [ImageSource]
    UniqueReviewAuthor storedReviewAuthor storedReviewOrder
    deriving Eq Show


-- * Orders

StoredOrder
    storedOrderCustomer StoredCustomerId
    storedOrderMeal StoredMealId
    storedOrderMenu StoredMenuId
    storedOrderChef StoredChefId
    storedOrderVolume Int
    storedOrderProgress OrderProgress
    storedOrderTime UTCTime
    deriving Eq Show


-- * Cart

CartRelation
    cartRelationCustomer StoredUserId
    cartRelationMeal StoredMealId
    cartRelationVolume Int
    cartRelationAdded UTCTime
    UniqueCartRelation cartRelationCustomer cartRelationMeal
    deriving Eq Show


-- * Tags

StoredChefTag
    chefTag ChefTag
    UniqueStoredChefTag chefTag
    deriving Eq Show

StoredCultureTag
    cultureTag CultureTag
    UniqueStoredCultureTag cultureTag
    deriving Eq Show

StoredDietTag
    dietTag DietTag
    UniqueStoredDietTag dietTag
    deriving Eq Show

StoredFarmTag
    farmTag FarmTag
    UniqueStoredFarmTag farmTag
    deriving Eq Show

StoredIngredientTag
    ingredientTag IngredientTag
    UniqueStoredIngredientTag ingredientTag
    deriving Eq Show

StoredMealTag
    mealTag MealTag
    UniqueStoredMealTag mealTag
    deriving Eq Show


-- * System State

PasswordSalt
    passwordSalt HashedPassword
    UniquePasswordSalt passwordSalt
    deriving Eq Show

NextImageSource
    nextImageSource ImageSource
    deriving Eq Show
|]



-- * Query Functions

-- ** Diets


insertIngredient :: Ingredient -> ReaderT SqlBackend (ResourceT IO) ()
insertIngredient (Ingredient name voids) = do
  mEnt <- getBy (UniqueStoredIngredientTag name)
  case mEnt of
    Nothing -> do
      insert_ (StoredIngredientTag name)
      void $ setViolations name voids
    Just _ ->
      void $ setViolations name voids



deleteIngredient :: IngredientTag -> ReaderT SqlBackend (ResourceT IO) ()
deleteIngredient name = do
  mEnt <- getBy (UniqueStoredIngredientTag name)
  case mEnt of
    Nothing -> pure ()
    Just (Entity k _) -> do
      delete k
      deleteWhere [IngredientDietViolationIngredientViolator ==. k]


getStoredIngredientTagId :: IngredientTag -> ReaderT SqlBackend (ResourceT IO) (Maybe StoredIngredientTagId)
getStoredIngredientTagId name = do
  mEnt <- getBy (UniqueStoredIngredientTag name)
  pure ((\(Entity k _) -> k) <$> mEnt)


getIngredientTagById :: StoredIngredientTagId -> ReaderT SqlBackend (ResourceT IO) (Maybe IngredientTag)
getIngredientTagById ingId = do
  mEnt <- get ingId
  pure $ (\(StoredIngredientTag t) -> t) <$> mEnt


getIngredientViolations :: StoredIngredientTagId -> ReaderT SqlBackend (ResourceT IO) [DietTag]
getIngredientViolations ingId = do
  xs <- selectList [IngredientDietViolationIngredientViolator ==. ingId] []
  fmap catMaybes $ forM xs $ \(Entity _ (IngredientDietViolation _ d)) ->
    getDietById d


getIngredientById :: StoredIngredientTagId -> ReaderT SqlBackend (ResourceT IO) (Maybe Ingredient)
getIngredientById ingId = do
  mName <- getIngredientTagById ingId
  case mName of
    Nothing -> pure Nothing
    Just name -> do
      voids <- getIngredientViolations ingId
      pure $ Just $ Ingredient name voids


getIngredientByName :: IngredientTag -> ReaderT SqlBackend (ResourceT IO) (Maybe Ingredient)
getIngredientByName ingName = do
  mIngId <- getStoredIngredientTagId ingName
  case mIngId of
    Nothing -> pure Nothing
    Just ingId -> getIngredientById ingId


getIngredients :: ReaderT SqlBackend (ResourceT IO) [Ingredient]
getIngredients = do
  xs <- selectList [] []
  fmap catMaybes $ forM xs $ \(Entity k _) ->
    getIngredientById k


insertDietTag :: DietTag -> ReaderT SqlBackend (ResourceT IO) ()
insertDietTag name =
  insert_ (StoredDietTag name)


registerViolation :: IngredientTag -> DietTag -> ReaderT SqlBackend (ResourceT IO) Bool
registerViolation name diet = do
  mIngId <- getStoredIngredientTagId name
  mDietId <- getDietId diet
  case (,) <$> mIngId <*> mDietId of
    Nothing -> pure False
    Just (ingId,dietId) -> do
      insert_ (IngredientDietViolation ingId dietId)
      pure True


setViolations :: IngredientTag -> [DietTag] -> ReaderT SqlBackend (ResourceT IO) Bool
setViolations name diets = do
  mIngId <- getStoredIngredientTagId name
  case mIngId of
    Nothing -> pure False
    Just ingId -> do
      oldDietIds <- fmap (fmap (\(Entity _ (IngredientDietViolation _ k)) -> k))
                  $ selectList [IngredientDietViolationIngredientViolator ==. ingId] []
      newDietIds <- fmap catMaybes $ forM diets getDietId
      let toRemove = Set.fromList oldDietIds `Set.difference` Set.fromList newDietIds
          toAdd = Set.fromList newDietIds `Set.difference` Set.fromList oldDietIds
      forM_ toRemove $ \dietId -> deleteWhere
        [ IngredientDietViolationIngredientViolator ==. ingId
        , IngredientDietViolationDietViolated ==. dietId
        ]
      forM_ toAdd $ \dietId -> insert_ (IngredientDietViolation ingId dietId)
      pure True


deleteDietTag :: DietTag -> ReaderT SqlBackend (ResourceT IO) ()
deleteDietTag tag = do
  mEnt <- getBy (UniqueStoredDietTag tag)
  case mEnt of
    Nothing -> pure ()
    Just (Entity k _) -> do
      delete k
      xs <- selectList [IngredientDietViolationDietViolated ==. k] []
      forM_ xs $ \(Entity k' _) -> delete k'


getDietId :: DietTag -> ReaderT SqlBackend (ResourceT IO) (Maybe StoredDietTagId)
getDietId tag = do
  mEnt <- getBy (UniqueStoredDietTag tag)
  pure ((\(Entity k _) -> k) <$> mEnt)


getDietById :: StoredDietTagId -> ReaderT SqlBackend (ResourceT IO) (Maybe DietTag)
getDietById tagId = do
  mEnt <- get tagId
  pure $ (\(StoredDietTag t) -> t) <$> mEnt



getDiets :: ReaderT SqlBackend (ResourceT IO) [DietTag]
getDiets = do
  xs <- selectList [] []
  pure $ (\(Entity _ (StoredDietTag x)) -> x) <$> xs


-- ** Semantics

getChefId :: Permalink -> ReaderT SqlBackend (ResourceT IO) (Maybe StoredChefId)
getChefId permalink = do
  mEnt <- getBy (UniqueChefPermalink permalink)
  case mEnt of
    Nothing -> pure Nothing
    Just (Entity k _) -> pure (Just k)


getMenuId :: Permalink -> Day -> ReaderT SqlBackend (ResourceT IO) (Maybe StoredMenuId)
getMenuId permalink deadline = do
  mChef <- getBy (UniqueChefPermalink permalink)
  case mChef of
    Nothing -> pure Nothing
    Just (Entity chefId _) -> do
      mEnt <- getBy (UniqueMenuDeadline chefId deadline)
      case mEnt of
        Nothing -> pure Nothing
        Just (Entity k _) -> pure (Just k)


getMealId :: Permalink -> Day -> Permalink -> ReaderT SqlBackend (ResourceT IO) (Maybe StoredMealId)
getMealId chefPermalink deadline mealPermalink = do
  mChef <- getBy (UniqueChefPermalink chefPermalink)
  case mChef of
    Nothing -> pure Nothing
    Just (Entity chefId _) -> do
      mMenu <- getBy (UniqueMenuDeadline chefId deadline)
      case mMenu of
        Nothing -> pure Nothing
        Just (Entity menuId _) -> do
          mEnt <- getBy (UniqueMealPermalink menuId mealPermalink)
          case mEnt of
            Nothing -> pure Nothing
            Just (Entity mealId _) -> pure (Just mealId)


-- ** Admin

addRole :: StoredUserId -> UserRole -> ReaderT SqlBackend (ResourceT IO) ()
addRole userId userRole = do
  mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
  case mUserRoleEnt of
    Just _ -> pure ()
    Nothing -> insert_ (UserRoleStored userRole userId)


delRole :: StoredUserId -> UserRole -> ReaderT SqlBackend (ResourceT IO) ()
delRole userId userRole = do
  mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
  case mUserRoleEnt of
    Nothing -> pure ()
    Just (Entity userRoleKey _) -> delete userRoleKey


hasRole :: StoredUserId -> UserRole -> ReaderT SqlBackend (ResourceT IO) Bool
hasRole userId userRole = do
  mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
  case mUserRoleEnt of
    Nothing -> pure False
    Just _  -> pure True


getRoles :: StoredUserId -> ReaderT SqlBackend (ResourceT IO) [UserRole]
getRoles userId = do
  userRoleEnts <- selectList [UserRoleStoredUserRoleOwner ==. userId] []
  pure ((\(Entity _ (UserRoleStored x _)) -> x) <$> userRoleEnts)


-- ** System

-- | Effectfully obtains the previous, unique image source name.
nextImageSource :: ReaderT SqlBackend (ResourceT IO) ImageSource
nextImageSource = do
  mEnt <- selectFirst [] []
  case mEnt of
    Nothing -> do
      insert_ (NextImageSource 1)
      pure 0
    Just (Entity imgSrcId (NextImageSource n)) -> do
      update imgSrcId [NextImageSourceNextImageSource =. (succ n)]
      pure n

getPasswordSalt :: ReaderT SqlBackend (ResourceT IO) HashedPassword
getPasswordSalt = do
  mSalt <- selectFirst [] []
  case mSalt of
    Nothing -> do
      newSalt <- HashedPassword <$> liftIO (randomByteString 32)
      insert_ (PasswordSalt newSalt)
      pure newSalt
    Just (Entity _ (PasswordSalt salt)) ->
      pure salt





-- Instances

instance Arbitrary StoredReviewId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredReviewId where
  hashWithSalt s x = hashWithSalt s (encode x)


instance Arbitrary StoredOrderId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredOrderId where
  hashWithSalt s x = hashWithSalt s (encode x)


instance Arbitrary StoredMealId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredMealId where
  hashWithSalt s x = hashWithSalt s (encode x)


instance Arbitrary StoredChefId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredChefId where
  hashWithSalt s x = hashWithSalt s (encode x)


instance Arbitrary StoredMenuId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredMenuId where
  hashWithSalt s x = hashWithSalt s (encode x)

instance Arbitrary StoredUserId where
  arbitrary = toSqlKey <$> arbitrary

instance Hashable StoredUserId where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField StoredUser typ) where
  x == y = case x of
    StoredUserCreated -> case y of
      StoredUserCreated -> True
    StoredUserPassword -> case y of
      StoredUserPassword -> True
    StoredUserEmail -> case y of
      StoredUserEmail -> True
    StoredUserConfirmed -> case y of
      StoredUserConfirmed -> True
    StoredUserId -> case y of
      StoredUserId -> True

instance ToJSON (EntityField StoredUser typ) where
  toJSON x = case x of
    StoredUserCreated -> String "created"
    StoredUserPassword -> String "password"
    StoredUserEmail -> String "email"
    StoredUserConfirmed -> String "confirmed"
    StoredUserId -> String "storedUserId"

instance FromJSON (EntityField StoredUser UTCTime) where
  parseJSON json = case json of
    String s
      | s == "created" -> pure StoredUserCreated
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json

instance FromJSON (EntityField StoredUser HashedPassword) where
  parseJSON json = case json of
    String s
      | s == "password" -> pure StoredUserPassword
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json

instance FromJSON (EntityField StoredUser EmailAddress) where
  parseJSON json = case json of
    String s
      | s == "email" -> pure StoredUserEmail
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json

instance FromJSON (EntityField StoredUser Bool) where
  parseJSON json = case json of
    String s
      | s == "confirmed" -> pure StoredUserConfirmed
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json

instance FromJSON (EntityField StoredUser StoredUserId) where
  parseJSON json = case json of
    String s
      | s == "storedUserId" -> pure StoredUserId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField StoredUser" json


instance Arbitrary StoredEditorId where
  arbitrary = toSqlKey <$> arbitrary


instance Hashable (Key UserRoleStored) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField UserRoleStored typ) where
  x == y = case x of
    UserRoleStoredUserRole -> case y of
      UserRoleStoredUserRole -> True
    UserRoleStoredUserRoleOwner -> case y of
      UserRoleStoredUserRoleOwner -> True
    UserRoleStoredId -> case y of
      UserRoleStoredId -> True

instance ToJSON (EntityField UserRoleStored typ) where
  toJSON x = case x of
    UserRoleStoredUserRole -> String "userRole"
    UserRoleStoredUserRoleOwner -> String "userRoleOwner"
    UserRoleStoredId -> String "userRoleStoredId"

instance FromJSON (EntityField UserRoleStored UserRole) where
  parseJSON json = case json of
    String s
      | s == "userRole" -> pure UserRoleStoredUserRole
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField UserRoleStored StoredUserId) where
  parseJSON json = case json of
    String s
      | s == "userRoleOwner" -> pure UserRoleStoredUserRoleOwner
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField UserRoleStored (Key UserRoleStored)) where
  parseJSON json = case json of
    String s
      | s == "userRoleStoredId" -> pure UserRoleStoredId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json


-- instance Arbitrary RecordSubmissionApprovalId where
--   arbitrary = toSqlKey <$> arbitrary


instance Hashable (Key PasswordSalt) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField PasswordSalt typ) where
  x == y = case x of
    PasswordSaltPasswordSalt -> case y of
      PasswordSaltPasswordSalt -> True
    PasswordSaltId -> case y of
      PasswordSaltId -> True

instance ToJSON (EntityField PasswordSalt typ) where
  toJSON x = case x of
    PasswordSaltPasswordSalt -> String "passwordSalt"
    PasswordSaltId -> String "passwordSaltId"

instance FromJSON (EntityField PasswordSalt HashedPassword) where
  parseJSON json = case json of
    String s
      | s == "passwordSalt" -> pure PasswordSaltPasswordSalt
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField PasswordSalt (Key PasswordSalt)) where
  parseJSON json = case json of
    String s
      | s == "passwordSaltId" -> pure PasswordSaltId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance Hashable (Key FacebookUserAccessTokenStored) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField FacebookUserAccessTokenStored typ) where
  x == y = case x of
    FacebookUserAccessTokenStoredFacebookUserAccessToken -> case y of
      FacebookUserAccessTokenStoredFacebookUserAccessToken -> True
    FacebookUserAccessTokenStoredFacebookUserDetails -> case y of
      FacebookUserAccessTokenStoredFacebookUserDetails -> True
    FacebookUserAccessTokenStoredId -> case y of
      FacebookUserAccessTokenStoredId -> True

instance ToJSON (EntityField FacebookUserAccessTokenStored typ) where
  toJSON x = case x of
    FacebookUserAccessTokenStoredFacebookUserAccessToken -> String "facebookUserAccessToken"
    FacebookUserAccessTokenStoredFacebookUserDetails -> String "facebookUserDetails"
    FacebookUserAccessTokenStoredId -> String "facebookUserAccessTokenStoredId"

instance FromJSON (EntityField FacebookUserAccessTokenStored FacebookUserAccessToken) where
  parseJSON json = case json of
    String s
      | s == "facebookUserAccessToken" -> pure FacebookUserAccessTokenStoredFacebookUserAccessToken
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserAccessTokenStored FacebookUserDetailsId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserDetails" -> pure FacebookUserAccessTokenStoredFacebookUserDetails
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserAccessTokenStored (Key FacebookUserAccessTokenStored)) where
  parseJSON json = case json of
    String s
      | s == "facebookUserAccessTokenStoredId" -> pure FacebookUserAccessTokenStoredId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance Hashable (Key FacebookUserDetails) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField FacebookUserDetails typ) where
  x == y = case x of
    FacebookUserDetailsFacebookUserId -> case y of
      FacebookUserDetailsFacebookUserId -> True
    FacebookUserDetailsFacebookUserOwner -> case y of
      FacebookUserDetailsFacebookUserOwner -> True
    FacebookUserDetailsId -> case y of
      FacebookUserDetailsId -> True

instance ToJSON (EntityField FacebookUserDetails typ) where
  toJSON x = case x of
    FacebookUserDetailsFacebookUserId -> String "facebookUserId"
    FacebookUserDetailsFacebookUserOwner -> String "facebookUserOwner"
    FacebookUserDetailsId -> String "facebookUserDetailsId"

instance FromJSON (EntityField FacebookUserDetails FacebookUserId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserId" -> pure FacebookUserDetailsFacebookUserId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserDetails StoredUserId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserOwner" -> pure FacebookUserDetailsFacebookUserOwner
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserDetails (Key FacebookUserDetails)) where
  parseJSON json = case json of
    String s
      | s == "facebookUserDetailsId" -> pure FacebookUserDetailsId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

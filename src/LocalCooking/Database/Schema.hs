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
import Control.Monad.IO.Class (liftIO)
import Database.Persist (Entity (..), (=.), (==.))
import Database.Persist.Class
  ( PersistEntity (Key, EntityField)
  , insert_, update, delete, deleteWhere, get, getBy, selectFirst, selectList
  )
import Database.Persist.Sql (SqlBackend, toSqlKey)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)
import Test.QuickCheck (Arbitrary (..))
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
    accessToken FacebookUserAccessToken
    userDetails FacebookUserDetailsId
    UniqueFacebookUserAccessToken accessToken
    UniqueFacebookUserAccessTokenOwner userDetails
    deriving Eq Show

FacebookUserDetails
    fbUserId FacebookUserId
    owner StoredUserId
    UniqueFacebookUserId fbUserId
    UniqueFacebookUserDetailsOwner owner
    deriving Eq Show

UserRoleStored
    role UserRole
    owner StoredUserId
    UniqueUserRole role owner
    deriving Eq Show

-- ** Customer User

StoredCustomer
    owner StoredUserId
    name Name
    address USAAddress
    UniqueCustomer owner
    deriving Eq Show

StoredDietPreference
    owner StoredCustomerId
    diet  StoredDietTagId
    UniqueDietPreference owner diet
    deriving Eq Show

StoredAllergy
    owner   StoredCustomerId
    allergy StoredIngredientTagId
    UniqueAllergy owner allergy
    deriving Eq Show

-- ** Chef User

StoredChef
    owner StoredUserId
    name Name
    permalink Permalink
    bio MarkdownText
    images [ImageSource]
    avatar ImageSource
    UniqueChefOwner owner
    UniqueChefPermalink permalink
    deriving Eq Show

ChefTagRelation
    chef StoredChefId
    tag StoredChefTagId
    UniqueChefTag chef tag
    deriving Eq Show

-- ** Farmer User

StoredFarmer
    owner StoredUserId
    name Name
    permalink Permalink
    images [ImageSource]
    avatar ImageSource
    bio MarkdownText
    UniqueFarmerOwner owner
    UniqueFarmerPermalink permalink
    deriving Eq Show

FarmTagRelation
    farmer StoredFarmerId
    tag StoredFarmTagId
    UniqueFarmTag farmer tag
    deriving Eq Show

-- ** Editor User

StoredEditor
    owner StoredUserId
    name Name
    UniqueEditor owner
    deriving Eq Show


-- * Meal

StoredMeal
    title Text
    permalink Permalink
    menu StoredMenuId
    heading Text
    description MarkdownText
    instructions MarkdownText
    images [ImageSource]
    price Price
    UniqueMealPermalink menu permalink
    deriving Eq Show

MealIngredient
    meal StoredMealId
    ingredient StoredIngredientTagId
    UniqueMealIngredient meal ingredient
    deriving Eq Show

MealTagRelation
    meal StoredMealId
    tag StoredMealTagId
    UniqueMealTag meal tag
    deriving Eq Show


-- * Menu

StoredMenu
    published Day Maybe
    deadline Day
    heading Text
    description MarkdownText
    images [ImageSource]
    author StoredChefId
    UniqueMenuDeadline author deadline
    deriving Eq Show

MenuTagRelation
    menu StoredMenuId
    tag StoredMealTagId
    UniqueMenuTag menu tag
    deriving Eq Show


-- * Diets

IngredientDietViolation
    violator StoredIngredientTagId
    violated StoredDietTagId
    UniqueViolation violator violated
    deriving Eq Show


-- * Review

StoredReview
    order StoredOrderId
    chef StoredChefId
    meal StoredMealId
    author StoredCustomerId
    rating Rating
    submitted UTCTime
    heading Text
    body MarkdownText
    images [ImageSource]
    UniqueReviewAuthor author order
    deriving Eq Show


-- * Orders

StoredOrder
    customer StoredCustomerId
    meal StoredMealId
    menu StoredMenuId
    chef StoredChefId
    volume Int
    progress OrderProgress
    time UTCTime
    deriving Eq Show


-- * Cart

CartRelation
    customer StoredUserId
    meal StoredMealId
    volume Int
    added UTCTime
    UniqueCartRelation customer meal
    deriving Eq Show


-- * Tags

StoredChefTag
    tag ChefTag
    UniqueStoredChefTag tag
    deriving Eq Show

StoredCultureTag
    tag CultureTag
    UniqueStoredCultureTag tag
    deriving Eq Show

StoredDietTag
    tag DietTag
    UniqueStoredDietTag tag
    deriving Eq Show

StoredFarmTag
    tag FarmTag
    UniqueStoredFarmTag tag
    deriving Eq Show

StoredIngredientTag
    tag IngredientTag
    UniqueStoredIngredientTag tag
    deriving Eq Show

StoredMealTag
    tag MealTag
    UniqueStoredMealTag tag
    deriving Eq Show


-- * System State

PasswordSalt
    salt HashedPassword
    UniquePasswordSalt salt
    deriving Eq Show

NextImageSource
    link ImageSource
    deriving Eq Show
|]



-- * Query Functions

-- ** Diets


insertIngredient :: Ingredient -> ReaderT SqlBackend IO ()
insertIngredient (Ingredient name voids) = do
  mEnt <- getBy (UniqueStoredIngredientTag name)
  case mEnt of
    Nothing -> do
      insert_ (StoredIngredientTag name)
      void $ setViolations name voids
    Just _ ->
      void $ setViolations name voids



deleteIngredient :: IngredientTag -> ReaderT SqlBackend IO ()
deleteIngredient name = do
  mEnt <- getBy (UniqueStoredIngredientTag name)
  case mEnt of
    Nothing -> pure ()
    Just (Entity k _) -> do
      delete k
      deleteWhere [IngredientDietViolationViolator ==. k]


getStoredIngredientTagId :: IngredientTag -> ReaderT SqlBackend IO (Maybe StoredIngredientTagId)
getStoredIngredientTagId name = do
  mEnt <- getBy (UniqueStoredIngredientTag name)
  pure ((\(Entity k _) -> k) <$> mEnt)


getIngredientTagById :: StoredIngredientTagId -> ReaderT SqlBackend IO (Maybe IngredientTag)
getIngredientTagById ingId = do
  mEnt <- get ingId
  pure $ (\(StoredIngredientTag t) -> t) <$> mEnt


getIngredientViolations :: StoredIngredientTagId -> ReaderT SqlBackend IO [DietTag]
getIngredientViolations ingId = do
  xs <- selectList [IngredientDietViolationViolator ==. ingId] []
  fmap catMaybes $ forM xs $ \(Entity _ (IngredientDietViolation _ d)) ->
    getDietById d


getIngredientById :: StoredIngredientTagId -> ReaderT SqlBackend IO (Maybe Ingredient)
getIngredientById ingId = do
  mName <- getIngredientTagById ingId
  case mName of
    Nothing -> pure Nothing
    Just name -> do
      voids <- getIngredientViolations ingId
      pure $ Just $ Ingredient name voids


getIngredientByName :: IngredientTag -> ReaderT SqlBackend IO (Maybe Ingredient)
getIngredientByName ingName = do
  mIngId <- getStoredIngredientTagId ingName
  case mIngId of
    Nothing -> pure Nothing
    Just ingId -> getIngredientById ingId


getIngredients :: ReaderT SqlBackend IO [Ingredient]
getIngredients = do
  xs <- selectList [] []
  fmap catMaybes $ forM xs $ \(Entity k _) ->
    getIngredientById k


insertDietTag :: DietTag -> ReaderT SqlBackend IO ()
insertDietTag name =
  insert_ (StoredDietTag name)


registerViolation :: IngredientTag -> DietTag -> ReaderT SqlBackend IO Bool
registerViolation name diet = do
  mIngId <- getStoredIngredientTagId name
  mDietId <- getDietId diet
  case (,) <$> mIngId <*> mDietId of
    Nothing -> pure False
    Just (ingId,dietId) -> do
      insert_ (IngredientDietViolation ingId dietId)
      pure True


setViolations :: IngredientTag -> [DietTag] -> ReaderT SqlBackend IO Bool
setViolations name diets = do
  mIngId <- getStoredIngredientTagId name
  case mIngId of
    Nothing -> pure False
    Just ingId -> do
      oldDietIds <- fmap (fmap (\(Entity _ (IngredientDietViolation _ k)) -> k))
                  $ selectList [IngredientDietViolationViolator ==. ingId] []
      newDietIds <- fmap catMaybes $ forM diets getDietId
      let toRemove = Set.fromList oldDietIds `Set.difference` Set.fromList newDietIds
          toAdd = Set.fromList newDietIds `Set.difference` Set.fromList oldDietIds
      forM_ toRemove $ \dietId -> deleteWhere
        [ IngredientDietViolationViolator ==. ingId
        , IngredientDietViolationViolated ==. dietId
        ]
      forM_ toAdd $ \dietId -> insert_ (IngredientDietViolation ingId dietId)
      pure True


deleteDietTag :: DietTag -> ReaderT SqlBackend IO ()
deleteDietTag tag = do
  mEnt <- getBy (UniqueStoredDietTag tag)
  case mEnt of
    Nothing -> pure ()
    Just (Entity k _) -> do
      delete k
      xs <- selectList [IngredientDietViolationViolated ==. k] []
      forM_ xs $ \(Entity k' _) -> delete k'


getDietId :: DietTag -> ReaderT SqlBackend IO (Maybe StoredDietTagId)
getDietId tag = do
  mEnt <- getBy (UniqueStoredDietTag tag)
  pure ((\(Entity k _) -> k) <$> mEnt)


getDietById :: StoredDietTagId -> ReaderT SqlBackend IO (Maybe DietTag)
getDietById tagId = do
  mEnt <- get tagId
  pure $ (\(StoredDietTag t) -> t) <$> mEnt



getDiets :: ReaderT SqlBackend IO [DietTag]
getDiets = do
  xs <- selectList [] []
  pure $ (\(Entity _ (StoredDietTag x)) -> x) <$> xs


-- ** Semantics

getChefId :: Permalink -> ReaderT SqlBackend IO (Maybe StoredChefId)
getChefId permalink = do
  mEnt <- getBy (UniqueChefPermalink permalink)
  case mEnt of
    Nothing -> pure Nothing
    Just (Entity k _) -> pure (Just k)


getMenuId :: Permalink -> Day -> ReaderT SqlBackend IO (Maybe StoredMenuId)
getMenuId permalink deadline = do
  mChef <- getBy (UniqueChefPermalink permalink)
  case mChef of
    Nothing -> pure Nothing
    Just (Entity chefId _) -> do
      mEnt <- getBy (UniqueMenuDeadline chefId deadline)
      case mEnt of
        Nothing -> pure Nothing
        Just (Entity k _) -> pure (Just k)


getMealId :: Permalink -> Day -> Permalink -> ReaderT SqlBackend IO (Maybe StoredMealId)
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

addRole :: StoredUserId -> UserRole -> ReaderT SqlBackend IO ()
addRole userId userRole = do
  mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
  case mUserRoleEnt of
    Just _ -> pure ()
    Nothing -> insert_ (UserRoleStored userRole userId)


delRole :: StoredUserId -> UserRole -> ReaderT SqlBackend IO ()
delRole userId userRole = do
  mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
  case mUserRoleEnt of
    Nothing -> pure ()
    Just (Entity userRoleKey _) -> delete userRoleKey


hasRole :: StoredUserId -> UserRole -> ReaderT SqlBackend IO Bool
hasRole userId userRole = do
  mUserRoleEnt <- getBy (UniqueUserRole userRole userId)
  case mUserRoleEnt of
    Nothing -> pure False
    Just _  -> pure True


getRoles :: StoredUserId -> ReaderT SqlBackend IO [UserRole]
getRoles userId = do
  userRoleEnts <- selectList [UserRoleStoredOwner ==. userId] []
  pure ((\(Entity _ (UserRoleStored x _)) -> x) <$> userRoleEnts)


-- ** System

-- | Effectfully obtains the previous, unique image source name.
nextImageSource :: ReaderT SqlBackend IO ImageSource
nextImageSource = do
  mEnt <- selectFirst [] []
  case mEnt of
    Nothing -> do
      insert_ (NextImageSource 1)
      pure 0
    Just (Entity imgSrcId (NextImageSource n)) -> do
      update imgSrcId [NextImageSourceLink =. (succ n)]
      pure n

getPasswordSalt :: ReaderT SqlBackend IO HashedPassword
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
    UserRoleStoredRole -> case y of
      UserRoleStoredRole -> True
    UserRoleStoredOwner -> case y of
      UserRoleStoredOwner -> True
    UserRoleStoredId -> case y of
      UserRoleStoredId -> True

instance ToJSON (EntityField UserRoleStored typ) where
  toJSON x = case x of
    UserRoleStoredRole -> String "userRole"
    UserRoleStoredOwner -> String "userRoleOwner"
    UserRoleStoredId -> String "userRoleStoredId"

instance FromJSON (EntityField UserRoleStored UserRole) where
  parseJSON json = case json of
    String s
      | s == "userRole" -> pure UserRoleStoredRole
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField UserRoleStored StoredUserId) where
  parseJSON json = case json of
    String s
      | s == "userRoleOwner" -> pure UserRoleStoredOwner
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



instance Hashable (Key PasswordSalt) where
  hashWithSalt salt x = hashWithSalt salt (toJSON x)

instance Eq (EntityField PasswordSalt typ) where
  x == y = case x of
    PasswordSaltSalt -> case y of
      PasswordSaltSalt -> True
    PasswordSaltId -> case y of
      PasswordSaltId -> True

instance ToJSON (EntityField PasswordSalt typ) where
  toJSON x = case x of
    PasswordSaltSalt -> String "passwordSalt"
    PasswordSaltId -> String "passwordSaltId"

instance FromJSON (EntityField PasswordSalt HashedPassword) where
  parseJSON json = case json of
    String s
      | s == "passwordSalt" -> pure PasswordSaltSalt
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
    FacebookUserAccessTokenStoredAccessToken -> case y of
      FacebookUserAccessTokenStoredAccessToken -> True
    FacebookUserAccessTokenStoredUserDetails -> case y of
      FacebookUserAccessTokenStoredUserDetails -> True
    FacebookUserAccessTokenStoredId -> case y of
      FacebookUserAccessTokenStoredId -> True

instance ToJSON (EntityField FacebookUserAccessTokenStored typ) where
  toJSON x = case x of
    FacebookUserAccessTokenStoredAccessToken -> String "facebookUserAccessToken"
    FacebookUserAccessTokenStoredUserDetails -> String "facebookUserDetails"
    FacebookUserAccessTokenStoredId -> String "facebookUserAccessTokenStoredId"

instance FromJSON (EntityField FacebookUserAccessTokenStored FacebookUserAccessToken) where
  parseJSON json = case json of
    String s
      | s == "facebookUserAccessToken" -> pure FacebookUserAccessTokenStoredAccessToken
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserAccessTokenStored FacebookUserDetailsId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserDetails" -> pure FacebookUserAccessTokenStoredUserDetails
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
    FacebookUserDetailsFbUserId -> case y of
      FacebookUserDetailsFbUserId -> True
    FacebookUserDetailsOwner -> case y of
      FacebookUserDetailsOwner -> True
    FacebookUserDetailsId -> case y of
      FacebookUserDetailsId -> True

instance ToJSON (EntityField FacebookUserDetails typ) where
  toJSON x = case x of
    FacebookUserDetailsFbUserId -> String "facebookUserId"
    FacebookUserDetailsOwner -> String "facebookUserOwner"
    FacebookUserDetailsId -> String "facebookUserDetailsId"

instance FromJSON (EntityField FacebookUserDetails FacebookUserId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserId" -> pure FacebookUserDetailsFbUserId
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "EntityField User" json

instance FromJSON (EntityField FacebookUserDetails StoredUserId) where
  parseJSON json = case json of
    String s
      | s == "facebookUserOwner" -> pure FacebookUserDetailsOwner
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

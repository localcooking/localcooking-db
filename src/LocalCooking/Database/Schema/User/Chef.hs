{-# LANGUAGE
    GADTs
  , QuasiQuotes
  , TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , OverloadedStrings
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Database.Schema.User.Chef where

import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Schema.Tag.Chef (StoredChefTagId)
import LocalCooking.Common.User.Name (Name)

import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Image.Source (ImageSource)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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
|]

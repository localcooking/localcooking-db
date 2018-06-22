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

module LocalCooking.Database.Schema.User.Farmer where

import LocalCooking.Database.Schema.User (StoredUserId)
import LocalCooking.Database.Schema.Tag.Farm (StoredFarmTagId)
import LocalCooking.Common.User.Name (Name)

import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Image.Source (ImageSource)
import Database.Persist.TH (share, persistLowerCase, mkPersist, sqlSettings, mkMigrate)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredFarmer
    storedFarmerOwner StoredUserId
    storedChefName Name
    storedChefPermalink Permalink
    storedChefImages [ImageSource]
    storedChefAvatar ImageSource
    storedChefBio MarkdownText
    UniqueFarmerOwner storedFarmerOwner
    UniqueChefPermalink storedChefPermalink
    deriving Eq Show

FarmTagRelation
    farmTagFarmer StoredFarmerId
    farmTagFarmTag StoredFarmTagId
    UniqueFarmTag farmTagFarmer farmTagFarmTag
    deriving Eq Show
|]

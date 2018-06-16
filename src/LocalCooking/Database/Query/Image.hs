module LocalCooking.Database.Query.Image where

import LocalCooking.Database.Schema.Image
  ( NextImageSource (..), EntityField (NextImageSourceNextImageSource))

import Data.Image.Source (ImageSource)
import Database.Persist (Entity (..), (=.))
import Database.Persist.Class (update, selectFirst, insert_)
import Database.Persist.Sql (ConnectionPool, runSqlPool)


-- | Effectfully obtains the previous, unique image source name.
nextImageSource :: ConnectionPool -> IO ImageSource
nextImageSource backend = do
  flip runSqlPool backend $ do
    mEnt <- selectFirst [] []
    case mEnt of
      Nothing -> do
        insert_ (NextImageSource 1)
        pure 0
      Just (Entity imgSrcId (NextImageSource n)) -> do
        update imgSrcId [NextImageSourceNextImageSource =. (succ n)]
        pure n

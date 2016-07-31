module Labtech.DB.Types where

import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock

import Labtech.IRC.Types
import Labtech.Types

data Idea
    = Idea
    { ideaKey :: Int
    , ideaText :: String
    }

data UploadEntry
    = UploadEntry
    { uploadKey :: Int
    , uploadUrl :: Url
    , uploadTitle :: Title
    , uploadFilepath :: FilePath
    , uploadUploadedTime :: UTCTime
    , uploadNick :: Nick
    }

instance FromRow Idea where
    fromRow = Idea
           <$> field
           <*> field

instance FromRow UploadEntry where
    fromRow  = UploadEntry
           <$> field
           <*> field
           <*> field
           <*> field
           <*> field
           <*> (Nick <$> field)

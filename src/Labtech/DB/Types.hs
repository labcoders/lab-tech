module Labtech.DB.Types where

import Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T
import Data.Time.Clock

import Labtech.IRC.Types
import Labtech.Types

data UploadEntry
    = UploadEntry
    { uploadUrl :: Url
    , uploadTitle :: Title
    , uploadFilepath :: FilePath
    , uploadUploadedTime :: UTCTime
    , uploadNick :: Nick
    }

instance FromRow UploadEntry where
    fromRow  = UploadEntry
           <$> field
           <*> field
           <*> field
           <*> field
           <*> (Nick <$> field)

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
           <$> (T.unpack <$> field)
           <*> (T.unpack <$> field)
           <*> (T.unpack <$> field)
           <*> field
           <*> (Nick <$> field)

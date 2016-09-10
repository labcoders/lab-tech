{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Labtech.DB where

import Control.Exception
import Control.Monad.IO.Class

import Data.Int
import Data.String

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import qualified Data.ByteString.Char8 as C

import Labtech.Types
import Labtech.DB.Pagination
import Labtech.DB.Queries
import Labtech.DB.Types
import Labtech.IRC.Types

defaultPageSize :: PageSize
defaultPageSize = 5

formatIdea :: IdeaEntry -> String
formatIdea (IdeaEntry { ideaKey = key , ideaText = txt })
  = show key ++ ". " ++ txt

-- | Pretty-print a format entry.
formatEntry :: UploadEntry -> String
formatEntry
  (UploadEntry
    { uploadKey = key
    , uploadUrl = url
    , uploadTitle = title
    , uploadNick = nick
    })
  = "(" ++ (show key) ++ ") - " ++ title ++ " (" ++ url ++ ") - uploaded by " ++ (unNick nick)

insertIdea :: String -> Nick-> DB String
insertIdea idea nick = withConnection $ \conn -> do
  r <- liftIO $ try $ execute conn ideaInsertStr (Only idea)
  case r of
    Left ex -> pure $
      "Failed to upload idea\"" ++ idea ++ " from " ++ (unNick nick) ++
      ". Exception was: " ++ (displayException (ex :: SqlError))
    Right _ -> pure $ "Got it, " ++ unNick nick ++ "."

-- | Counts the rows in the given column.
--
-- /Warning:/ this constructs a SQL query by splicing strings!
countRows :: String -> DB (Either SqlError Int)
countRows col = withConnection $ \conn -> do
  let q = fromString $ "SELECT COUNT(1) FROM " ++ col
  res <- liftIO $ try (query_ conn q)
  pure $ (fromOnly . head <$> res)

paginateListUploads :: PageSpec -> DB (Either SqlError (Page UploadEntry))
paginateListUploads info = withConnection $ \conn -> do
  let q = unPaginatedListQuery paginatedListUploadsQuery
  res <- liftIO $ try $ query conn q (pageInfoParams info)
  rowsE <- countRows "uploads"
  pure $ do
    items <- res
    rows <- rowsE
    pure Page
      { pageSpec = info
      , pageCount = rows
      , pageData = items
      }

-- | Delete a datum by ID using the given query.
deleteFrom :: DeleteQuery -> Int -> DB String
deleteFrom (DeleteQuery q) i = withConnection $ \conn -> do
  res <- liftIO $ try $ execute conn q $ Only i :: DB (Either SqlError Int64)
  case res of
    Left ex ->
      pure $ "Failed to delete. Exception was: " ++ displayException ex
    Right _ -> pure "Done"

ideaTableContains :: String -> DB Bool
ideaTableContains s = withConnection $ \conn -> do
  is <- liftIO $ query conn ideaContainsStr (Only s) :: DB [UploadEntry]
  pure $ length is /= 0

uploadTableContains :: String -> DB Bool
uploadTableContains s = withConnection $ \conn -> do
  urls <- liftIO $ query conn uploadContainsStr (toField ("url" :: String), toField s) :: DB [UploadEntry]
  tits <- liftIO $ query conn uploadContainsStr (toField ("title" :: String), toField s) :: DB [UploadEntry]
  pure $ length urls /= 0 || length tits /= 0

queryUploads :: DB [UploadEntry]
queryUploads = withConnection $ \conn -> liftIO $ query_ conn uploadSelectAllStr

listIdeas :: DB [IdeaEntry]
listIdeas = withConnection $ \conn -> liftIO $ query_ conn ideaSelectAllStr

getUpload :: String -> DB (Maybe String)
getUpload s = withConnection $ \conn -> do
  items <- liftIO $ query conn uploadSelectStr (Only (s :: String))
  case items of
    [] -> pure Nothing
    (x:_) -> pure (Just x)

insertUpload :: Url -> Title -> FilePath -> Nick -> DB String
insertUpload url tit fp nick = withConnection $ \conn -> do
  q <- liftIO $ formatQuery conn uploadInsertStr ((C.pack url :: C.ByteString), tit :: String, (C.pack fp :: C.ByteString), (unNick nick) :: String)
  liftIO $ print q
  r <- (liftIO $ try $ execute conn uploadInsertStr ((C.pack url :: C.ByteString), tit :: String, (C.pack fp :: C.ByteString), (unNick nick) :: String)) :: DB (Either SqlError Int64)
  case r of
    Left ex -> pure $
      "Failed to upload \"" ++ tit ++ "\" (" ++ url ++ ") from " ++
      unNick nick ++ ". Exception was: " ++ displayException ex
    Right _ -> pure $
      "Successfully uploaded \"" ++ tit ++ "\" (" ++ url ++ ") from " ++
      unNick nick

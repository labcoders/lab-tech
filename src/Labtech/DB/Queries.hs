{-# LANGUAGE OverloadedStrings #-}

module Labtech.DB.Queries where

import Labtech.Command.Types

import Database.PostgreSQL.Simple

newtype PaginatedListQuery
  = PaginatedListQuery
    { unPaginatedListQuery :: Query
    }

newtype DeleteQuery
  = DeleteQuery
    { unDeleteQuery :: Query
    }

-- | Query to select all uploads.
uploadSelectAllStr :: Query
uploadSelectAllStr
  = "SELECT id, url, title, filepath, uploadtime, nick FROM uploads"

-- | Query to select an upload according to a title.
uploadSelectStr :: Query
uploadSelectStr = "select id, url, title, filepath, uploadtime, nick from uploads where title = ?"

-- | Query to insert an upload.
uploadInsertStr :: Query
uploadInsertStr = "insert into uploads (url, title, filepath, uploadtime, nick) values (?, ?, ?, DEFAULT, ?)"

-- | Query to select an upload based on equality of one given column.
uploadContainsStr :: Query
uploadContainsStr = "select id, url, title, filepath, uploadtime, nick from uploads where ? = ?"

-- | Query to select all ideas.
ideaSelectAllStr :: Query
ideaSelectAllStr = "SELECT id, idea FROM ideas"

-- | Query to search for an idea with a SQL /LIKE/ check.
ideaContainsStr :: Query
ideaContainsStr = "select idea from ideas where idea like ?"

-- | Query to insert an idea.
ideaInsertStr :: Query
ideaInsertStr = "insert into ideas (idea) values (?)"

deleteUploadsQuery :: Query
deleteUploadsQuery = "delete from uploads where id = ?"

deleteIdeasQuery :: Query
deleteIdeasQuery = "delete from ideas where id = ?"

paginatedListUploadsQuery :: PaginatedListQuery
paginatedListUploadsQuery = PaginatedListQuery
  "SELECT id, url, title, filepath, uploadtime, nick \
  \FROM uploads \
  \ORDER BY uploadtime DESC \
  \LIMIT ? \
  \OFFSET ?"

paginatedListIdeasQuery :: PaginatedListQuery
paginatedListIdeasQuery = PaginatedListQuery
  "SELECT id, idea \
  \FROM ideas \
  \ORDER BY uploadtime DESC \
  \LIMIT ? \
  \OFFSET ?"

deleteQueryFor :: ListTarget -> DeleteQuery
deleteQueryFor t = DeleteQuery $ case t of
  ListUploads -> deleteUploadsQuery
  ListIdeas -> deleteIdeasQuery

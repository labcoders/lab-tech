{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Labtech.DB.Types where

import Labtech.Command.Types ( ListTarget(..) )
import Labtech.IRC.Types
import Labtech.Types

import Control.Concurrent.Chan
import Control.Monad.Reader
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock

defaultConnectionPoolSize :: Int
defaultConnectionPoolSize = 20

labtechConnInfo :: ConnectInfo
labtechConnInfo = defaultConnectInfo
  { connectUser = "labtech"
  , connectDatabase = "labtech"
  }

data ConnectionPool
  = ConnectionPool
    { connections :: Chan Connection
    , currentConnection :: Maybe Connection
    }

-- | Make a connection pool of the given size and with no ambient connection.
makeConnectionPool :: Int -> ConnectInfo -> IO ConnectionPool
makeConnectionPool n info = do
  chan <- newChan
  conns <- mapM (const (connect info)) [1..n]
  forM_ conns $ \conn -> do
    writeChan chan conn
  pure $ ConnectionPool { connections = chan, currentConnection = Nothing }

runDB :: ConnectionPool -> DB a -> IO a
runDB pool (DB m) = runReaderT m pool

newtype DB a
  = DB
    { unDB :: ReaderT ConnectionPool IO a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

withCurrentConn :: Connection -> ConnectionPool -> ConnectionPool
withCurrentConn conn pool = pool { currentConnection = Just conn }

withNoCurrentConn :: ConnectionPool -> ConnectionPool
withNoCurrentConn pool = pool { currentConnection = Nothing }

-- | Runs a database action without an ambient connection.
withoutConnection :: DB a -> DB a
withoutConnection (DB m) = DB (local withNoCurrentConn m)

-- | Runs a database action with the ambient connection. If there is no ambient
-- connection, then a connection is acquired from the pool first, which blocks
-- execution. In either case, the given database action will have its ambient
-- connection set, so nested calls to @withConnection@ do not block.
--
-- See 'withoutConnection'.
withConnection :: (Connection -> DB a) -> DB a
withConnection f = do
  -- check whether we have an ambient connection
  m <- DB (asks currentConnection)
  conn <- case m of
    Nothing -> do
      -- if there is no ambient connection, then pull one from the pool
      chan <- DB (asks connections)
      liftIO $ readChan chan
    Just conn -> pure conn -- otherwise use the ambient connection

  -- run the given function, ensuring that the ambient connection is set
  x <- DB (local (withCurrentConn conn) (unDB $ f conn))

  case m of
    Nothing -> do
      -- write back the connection to the pool if we had pulled one from it
      chan <- DB (asks connections)
      liftIO $ writeChan chan conn
    Just _ -> pure () -- otherwise do nothing

  pure x

-- | Provides a new connection for running a database action. The connection is
-- guaranteed to be new in the sense that if there is an ambient connection,
-- then that is /not/ the one that will be provided to the function; a new
-- connection will be acquired from the pool.
--
-- /Warning:/ this is deadlock prone if there is potentially unbounded nesting
-- of calls to this function!
withNewConnection :: (Connection -> DB a) -> DB a
withNewConnection f = withoutConnection (withConnection f)

type family AssociatedModel (a :: ListTarget) :: * where
  AssociatedModel 'ListUploads = UploadEntry
  AssociatedModel 'ListIdeas = IdeaEntry

data Model
  = IdeaModel IdeaEntry
  | UploadModel UploadEntry

data IdeaEntry
    = IdeaEntry
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

instance FromRow IdeaEntry where
    fromRow = IdeaEntry
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

{-# LANGUAGE FlexibleInstances #-}

module Labtech.IRC.Types where

import Control.Monad.Reader
import qualified Data.Map as M

-- | Unique identifier for workers.
newtype ServerName = ServerName { unWorkerName :: String }
  deriving (Eq, Ord, Show)

-- | Target for message replication.
data ReplicationTarget
  = ReplicationTarget
    { replicationServer :: ServerName
    -- ^ The name of the server to replicate the message to.
    , replicationTarget :: MessageTarget
    -- ^ The channel or nick to send the message to on that server.
    }

-- | Nickname of an IRC user.
newtype Nick = Nick { unNick :: String }
  deriving (Eq, Ord, Show)

-- | Username of an IRC user.
newtype Username = Username { unUsername :: String }
  deriving (Eq, Ord, Show)

-- | Real name of an IRC user.
newtype RealName = RealName { unRealName :: String }
  deriving (Eq, Ord, Show)

-- | An IRC channel, including the @#@.
newtype Channel = Channel { unChannel :: String }
  deriving (Eq, Ord, Show)

newtype Ping = Ping { unPing :: String }
  deriving (Eq, Ord, Show)

-- | The target of a privmsg.
data MessageTarget
  = ChannelTarget Channel
  -- ^ The target of the message is a channel.
  | NickTarget Nick
  -- ^ The target of the message is a specific user.
  deriving (Eq, Ord, Show)

-- | Convert a message target into an 'Either'.
targetToEither :: MessageTarget -> Either Channel Nick
targetToEither m = case m of
  ChannelTarget x -> Left x
  NickTarget x -> Right x

-- | Extract the inner string of the target regardless of what kind of target
-- it is.
renderTarget :: MessageTarget -> String
renderTarget = either unChannel unNick . targetToEither

-- | The origin of a message, which is the first part of an IRC @PRIVMSG@.
data MessageOrigin
  = MessageOrigin
    { originNick :: Nick
    -- ^ The nickname of the sender.
    , originHost :: String
    -- ^ The hostname of the sender.
    }
  deriving (Eq, Ord, Show)

data Message
  = Privmsg MessageOrigin MessageTarget String
  | Pingmsg Ping

-- | A specification of an IRC server connection. This includes the nicknames
-- that labtech will use on the server as well as the channels that labtech
-- will connect to.
data ServerSpec
  = ServerSpec
    { serverNicks :: [Nick]
    , serverHost :: String
    , serverPort :: Int
    , serverChannels :: [Channel]
    , serverUsername :: Username
    , serverRealName :: RealName
    , serverWorkerName :: ServerName
    , serverReplication :: M.Map Channel [ReplicationTarget]
    }

-- | Concrete 'IO'-based interpreter for IRC commands.
data IrcEnv
  = IrcEnv
    { _nickE :: Nick -> IO ()
    , _userE :: Username -> RealName -> IO ()
    , _joinE :: Channel -> IO ()
    , _pongE :: Ping -> IO ()
    , _nextE :: IO Message
    , _privmsgE :: MessageTarget -> String -> IO ()
    }

-- | Class of monads capable of performing IRC actions.
class MonadIRC m where
  ircNick :: Nick -> m ()
  ircUser :: Username -> RealName -> m ()
  ircJoin :: Channel -> m ()
  ircPong :: Ping -> m ()
  ircNext :: m Message
  ircPrivmsg :: MessageTarget -> String -> m ()

instance MonadIO m => MonadIRC (ReaderT IrcEnv m) where
  ircNick nick = do
    f <- asks _nickE
    liftIO $ f nick
  ircUser user real = do
    f <- asks _userE
    liftIO $ f user real
  ircJoin chan = do
    f <- asks _joinE
    liftIO $ f chan
  ircPong ping = do
    f <- asks _pongE
    liftIO $ f ping
  ircNext = liftIO =<< asks _nextE
  ircPrivmsg target msg = do
    f <- asks _privmsgE
    liftIO $ f target msg

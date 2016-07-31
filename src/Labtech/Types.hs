{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Labtech.Types where

import Labtech.IRC.Types
import Labtech.Help ( help )

import Control.Monad.Reader
import Control.Monad.State

-- | A constraint in which the monad @t n@ can perform IRC actions as well as
-- IO actions.
type MonadIrcIO t n = (MonadTrans t, Monad n, Monad (t n), MonadIO n, MonadIRC (t n))

type MonadLabIrcIO m = (Monad m, MonadIO m, MonadLab m, MonadIRC m)

type Url = String
type Title = String

type FileListing = [String]

-- | The Labtech monad.
newtype LabtechT m a =
  Labtech
    { unLabtechT :: StateT LabtechState (ReaderT LabtechEnv m) a
    }

type Labtech = LabtechT IO

data LabtechState
  = LabtechState
    { labNextNicks :: [Nick]
    }

data LabtechEnv
  = LabtechEnv
    { labIrcEnv :: IrcEnv
    , labLabEnv :: LabEnv
    , labSpec :: ServerSpec
    }

-- | Gets the next nick in a state monad over 'LabtechState', such as
-- @'LabtechT' m@.
nextNick :: MonadState LabtechState m => m Nick
nextNick = do
  (nick:nicks) <- gets labNextNicks
  modify $ \s -> s { labNextNicks = nicks }
  pure nick

deriving instance Functor m => Functor (LabtechT m)
deriving instance Monad m => Applicative (LabtechT m)
deriving instance Monad m => Monad (LabtechT m)
deriving instance MonadIO m => MonadIO (LabtechT m)
deriving instance Monad m => MonadReader LabtechEnv (LabtechT m)
deriving instance Monad m => MonadState LabtechState (LabtechT m)

instance MonadTrans LabtechT where
  lift = Labtech . lift . lift

-- | Runs a 'Labtech' computation in IO using the given concrete labtech and
-- IRC interpreters.
runLabtech :: ServerSpec -> IrcEnv -> LabEnv -> Labtech a -> IO a
runLabtech spec irc lab m = runReaderT (evalStateT mtl initial) env where
  mtl = unLabtechT m
  initial = LabtechState
    { labNextNicks = cycle (serverNicks spec)
    }
  env = LabtechEnv
    { labIrcEnv = irc
    , labLabEnv = lab
    , labSpec = spec
    }

subLabtech :: Labtech a -> Labtech (IO (a, LabtechState))
subLabtech m = do
  s <- get
  r <- ask
  pure $ runReaderT (runStateT (unLabtechT m) s) r

-- | Class of monads capable of performing labtech actions.
class MonadLab m where
  labGetSpec :: m ServerSpec
  labPreferNicks :: [Nick] -> m ()
  -- | Tries to set the nick to the current most preferred nick.
  labRenick :: m ()

instance (MonadIO m, Monad m) => MonadLab (LabtechT m) where
  labGetSpec = asks labSpec
  labPreferNicks nicks = modify $ \s -> s { labNextNicks = nicks ++ labNextNicks s }
  labRenick = ircNick =<< ircNextNick

-- | Concrete 'IO'-based interpreter for Labtech commands.
data LabEnv
  = LabEnv
    { _listE
        :: forall t n. MonadIrcIO t n => MessageTarget -> t n FileListing
    , _uploadE
        :: forall t n. MonadIrcIO t n => Url -> FilePath -> MessageTarget -> t n ()
    , _helpE
        :: forall t n. MonadIrcIO t n => MessageTarget -> t n ()
    }

instance (Monad m, MonadIO m) => MonadIRC (LabtechT m) where
  ircNick nick = do
    f <- asks (_nickE . labIrcEnv)
    liftIO $ f nick
  ircUser user name = Labtech $ do
    f <- asks (_userE . labIrcEnv)
    liftIO $ f user name
  ircJoin chan = Labtech $ do
    f <- asks (_joinE . labIrcEnv)
    liftIO $ f chan
  ircPong ping = Labtech $ do
    f <- asks (_pongE . labIrcEnv)
    liftIO $ f ping
  ircNext = Labtech $ do
    f <- asks (_nextE . labIrcEnv)
    liftIO f
  ircPrivmsg chan msg = Labtech $ do
    f <- asks (_privmsgE . labIrcEnv)
    liftIO $ f chan msg
  ircNextNick = nextNick

simpleLabEnv :: LabEnv
simpleLabEnv = LabEnv
  { _helpE = \target -> forM_ help $ \line -> do
      ircPrivmsg target line
  , _listE = error "List unsupported."
  , _uploadE = error "Upload unsupported."
  }

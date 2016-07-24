{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Labtech.Types where

import Labtech.IRC.Types
import Labtech.Help ( help )

import Control.Concurrent.Chan
import Control.Monad.Reader

-- | A constraint in which the monad @t n@ can perform IRC actions as well as
-- IO actions.
type MonadIrcIO t n = (MonadTrans t, Monad n, Monad (t n), MonadIO n, MonadIRC (t n))

type Url = String

type FileListing = [String]

-- | The Labtech monad.
type Labtech = ReaderT (IrcEnv, LabEnv) IO

-- | Runs a 'Labtech' computation in IO using the given concrete labtech and
-- IRC interpreters.
runLabtech :: IrcEnv -> LabEnv -> Labtech a -> IO a
runLabtech irc lab m = runReaderT m (irc, lab)

-- | Class of monads capable of performing labtech actions.
class MonadLab m where
  labList :: MessageTarget -> m ()
  labUpload :: Url -> FilePath -> MessageTarget -> m ()
  labHelp :: MessageTarget -> m ()

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

instance MonadIO m => MonadIRC (ReaderT (IrcEnv, LabEnv) m) where
  ircNick = withReaderT fst . ircNick
  ircUser = withReaderT fst .% ircUser where (.%) = (.) . (.)
  ircJoin = withReaderT fst . ircJoin
  ircPong = withReaderT fst . ircPong
  ircNext = withReaderT fst ircNext
  ircPrivmsg = withReaderT fst .% ircPrivmsg where (.%) = (.) . (.)

instance MonadIO m => MonadLab (ReaderT (IrcEnv, LabEnv) m) where
  labHelp target = do
    f <- asks (_helpE . snd)
    f target
  labUpload = error "unimplemented: upload"
  labList = error "unimplemented: list"

simpleLabEnv :: LabEnv
simpleLabEnv = LabEnv
  { _helpE = \target -> forM_ help $ \line -> do
      ircPrivmsg target line
  , _listE = error "List unsupported."
  , _uploadE = error "Upload unsupported."
  }

-- | A pair of channels for bidirectional communication.
data Bichan i o
  = Bichan
    { readSide :: !(Chan i)
    , writeSide :: !(Chan o)
    }

type Bichan' a = Bichan a a

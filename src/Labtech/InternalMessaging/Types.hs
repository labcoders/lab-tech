module Labtech.InternalMessaging.Types where

import Control.Concurrent.Chan

import Labtech.IRC.Types hiding ( Privmsg )
import Labtech.Orphans ()

type WorkerName = ServerName

data InternalMessage sender body
  = InternalMessage
    { intmsgSender :: !sender
    , intmsgBody :: !body
    }

-- | Internal messages sent from the server to a worker.
type InternalMessageSW = InternalMessage () InternalMessageBodySend

-- | Internal messages sent from a worker to the server.
type InternalMessageWS = InternalMessage WorkerName InternalMessageBodyRecv

-- | Bodies of internal messages received by the server.
data InternalMessageBodyRecv
  = InterServerPrivmsg
    WorkerName -- ^ Destination server worker name
    MessageTarget -- ^ Desired recipient of message on remote server
    MessageTarget -- ^ Original recipient of message
    MessageOrigin -- ^ Original sender
    String -- ^ Message body
  deriving (Eq, Ord, Show)

data RegistrationStatus
  -- | The registration of the worker was successful.
  = RegistrationOk !(Chan InternalMessageWS)
  -- | The registration of the worker failed.
  | RegistrationFailed RegistrationFailureReason
  deriving (Eq, Show)

-- | Reasons for the registration failure of a worker.
data RegistrationFailureReason
  -- | The desired name of the worker is already taken.
  = WorkerNameInUse
  -- | Another reason not covered above is included as a free-form string.
  | OtherFailure String
  deriving (Eq, Ord, Show)

-- | Bodies of internal messages sent by the server.
data InternalMessageBodySend
  -- | Send an IRC privmsg to the given destination.
  = Privmsg
    MessageTarget -- ^ The target of the message, either a channel or a nick.
    String -- ^ The message to send.
  -- | Join a new channel.
  | Join
    Channel -- ^ The channel to join
  -- | A trivial message sen
  | RegistrationResult RegistrationStatus
  deriving (Eq, Show)

-- | A registration request that a worker can send to the server to participate
-- in the inter-server messaging system.
data WorkerRegistration
  = WorkerRegistration
    WorkerName
    (Chan InternalMessageSW)
  | WorkerUnregistration
    WorkerName

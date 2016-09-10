{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Labtech.Command.Types where

import Labtech.IRC.Types

data ListTarget
  = ListUploads
  | ListIdeas
  deriving (Eq, Ord, Read, Show)

data CommandEnv command
  = CommandEnv
    { commandSender :: Nick
    , commandBody :: command
    , commandTarget :: MessageTarget
    , commandServerSpec :: ServerSpec
    }
  deriving (Functor)

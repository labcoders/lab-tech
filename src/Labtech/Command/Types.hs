{-# LANGUAGE DeriveFunctor #-}

module Labtech.Command.Types where

import Labtech.IRC.Types

data CommandEnv command
  = CommandEnv
    { commandSender :: Nick
    , commandBody :: command
    , commandTarget :: MessageTarget
    , commandServerSpec :: ServerSpec
    }
  deriving (Functor)

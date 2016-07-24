module Labtech.IRC.Servers ( servers ) where

import Labtech.IRC.Types

servers :: [ServerSpec]
servers = [ labcodersSpec, freenodeSpec ]

labcodersSpec :: ServerSpec
labcodersSpec = ServerSpec
  { serverNicks = Nick <$> [ "labtech", "labtech_", "labtech__" ]
  , serverUsername = Username "labtech"
  , serverRealName = RealName "labtech"
  , serverHost = "labcoders.club"
  , serverPort = 6667
  , serverChannels = Channel <$> [ "#general" ]
  }

freenodeSpec :: ServerSpec
freenodeSpec = ServerSpec
  { serverNicks = Nick <$> [ "labtech", "labtech_", "labtech__" ]
  , serverHost = "irc.freenode.net"
  , serverUsername = Username "labtech"
  , serverRealName = RealName "labtech"
  , serverPort = 6667
  , serverChannels = Channel <$> [ "#labcoders" ]
  }


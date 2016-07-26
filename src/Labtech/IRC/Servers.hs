module Labtech.IRC.Servers ( servers ) where

import Labtech.IRC.Types

import qualified Data.Map as M

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
  , serverReplication = M.fromList
    [ ( Channel "#general"
      , [ ReplicationTarget
          (ServerName "freenode")
          (ChannelTarget (Channel "#labcoders"))
        ]
      )
    ]
  , serverWorkerName = ServerName "labcoders"
  }

freenodeSpec :: ServerSpec
freenodeSpec = ServerSpec
  { serverNicks = Nick <$> [ "labtech", "labtech_", "labtech__" ]
  , serverHost = "irc.freenode.net"
  , serverUsername = Username "labtech"
  , serverRealName = RealName "labtech"
  , serverPort = 6667
  , serverChannels = Channel <$> [ "#labcoders" ]
  , serverWorkerName = ServerName "freenode"
  , serverReplication = M.fromList
    [ ( Channel "#labcoders"
      , [ ReplicationTarget
          (ServerName "labcoders")
          (ChannelTarget (Channel "#general"))
        ]
      )
    ]
  }


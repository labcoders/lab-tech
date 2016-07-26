module Labtech.Orphans where

import Control.Concurrent.Chan

instance Show (Chan a) where
  show _ = "<chan>"

module Labtech.Bichan where

import Control.Concurrent.Chan

-- | A pair of channels for bidirectional communication.
data Bichan i o
  = Bichan
    { readSide :: !(Chan i)
    , writeSide :: !(Chan o)
    }

-- | Write a value into the sending end of a 'Bichan'.
writeBichan :: Bichan i o -> o -> IO ()
writeBichan chan = writeChan (writeSide chan)

-- | Read a value from the receiving end of a 'Bichan'.
readBichan :: Bichan i o -> IO i
readBichan chan = readChan (readSide chan)

-- | Flips the reading and writing ends of a 'Bichan'.
--
-- Useful when giving a Bichan to another thread, from whose perspective the
-- read/write sides are reversed.
flipBichan :: Bichan i o -> Bichan o i
flipBichan b = Bichan { readSide = writeSide b, writeSide = readSide b }

-- | A bidirectional channel in which both sides send and receive the same type
-- of data.
type Bichan' a = Bichan a a

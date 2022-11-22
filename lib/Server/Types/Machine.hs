-- | Types used by all Machine implementations
module Server.Types.Machine where

import PlunderPrelude

import Server.Types.Logging

data MachineEvent
    -- | Have the machine take a snapshot and exit its async.
    = MachineEventSnapshotAndShutdown
    -- | Halt right now.
    | MachineEventImmediateShutdown
  deriving (Show)

data MachineHandle = MachineHandle
    { thControlQ :: TQueue MachineEvent
      -- ^ Sends messages to the Machine.

    , thAsync :: Async ()
      -- ^ Async for the machine thread. Exits on request with the shutdown
      -- MachineEvent.
    }

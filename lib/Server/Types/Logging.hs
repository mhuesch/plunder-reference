{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.Types.Logging where

import PlunderPrelude

import Numeric.Natural (Natural)
import Plun            (Fan)
import Servant         (FromHttpApiData, ToHttpApiData)

import qualified Data.Aeson as A

--------------------------------------------------------------------------------

-- | A machine, a set of related processes, are given a human readable name.
newtype CogName = COG_NAME { txt :: Text }
  deriving newtype (Show, Eq, Ord, A.ToJSON, A.FromJSON)
  deriving newtype (A.ToJSONKey, A.FromJSONKey)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A positional index into the machine's Request vector.
newtype RequestIdx = RequestIdx { int :: Int }
  deriving newtype (Eq, Show, Ord)

-- | A record of one call to a Process and its side effects.
data Receipt
  -- | Receipt of a normal Eval that completed successfully. Since everything
  -- is deterministic, the eval can just be replayed for its value during
  -- replay.
  = ReceiptEvalOK RequestIdx

  -- | Receipt of anything else.
  | ReceiptVal RequestIdx Fan
  deriving (Show)

-- | Log batches count up from 0.
newtype BatchNum = BatchNum { unBatchNum :: Natural }
  deriving newtype (Eq, Show, Num)

-- | The atomic unit written to the event log. A Plunder Machine should be able
-- to be restored from a sequence of LogBatches which are an execution record.
data LogBatch = LogBatch {
  -- | Monotonically increasing id.
  batchNum     :: BatchNum,

  -- | Time the LogBatch was written.
  writeTime    :: Natural,

  -- | The batch number of the previous snapshot.
  --
  -- Since event logs are a map from a `BatchNum` to a `LogBatch`, this is used
  -- to quickly rewind backwards through the event log to the previous
  -- snapshot.
  lastSnapshot :: BatchNum,

  -- | Not every log batch has a snapshot, but when they do, it is enough to
  -- entirely restart the system from this snapshot. It is safe to delete all
  -- prevoius LogBatches before a LogBatch with a snapshot.
  snapshot     :: Maybe Fan,

  -- | Events which were run this batch. If there's a snapshot in this LogBatch,
  -- these are run on top of the snapshot state.
  executed     :: [Receipt]
  }
  deriving (Show)

-- -----------------------------------------------------------------------

-- Interface to the logging system.

data LoadMachine
  = NewMachine
  | ExistingMachine BatchNum

data ReplayFrom
  -- | Play from the beginning.
  = EarliestBatch
  -- | Replay from the latest marked snapshot.
  | LatestSnapshot

{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.EvalMachine where

import Optics                hiding ((%%))
import PlunderPrelude
import Server.Convert
import Server.Hardware.Types
import Server.LmdbStore
import Server.Time
import Server.Types.Logging
import Server.Types.Machine
import Server.Util
import Server.Debug

import Control.Monad.State (StateT, evalStateT)
import GHC.Conc            (numCapabilities)
import Plun                (Fan(..), UserError(..), boom, getRow, mkRow, (%%))

import qualified Rex

import qualified Data.Map      as M
import qualified Data.Sequence as Q
import qualified Data.Vector   as V
import qualified Loot.ReplExe


-- This is a new Machine implementation which only deals with three requests:
-- Eval and Call.
--
-- EVAL: A request to evaluate a piece of plunder with a timeout.
-- CALL: Calls a piece of hardware.

oneSecondInUs :: Nat
oneSecondInUs = 10 ^ (6::Int)

fiveSecondsInUs :: Nat
fiveSecondsInUs = 5 * oneSecondInUs

thirtySecondsInUs :: Nat
thirtySecondsInUs = 30 * oneSecondInUs

twoMinutesInUs :: Nat
twoMinutesInUs = 2 * 60 * oneSecondInUs

data EvalResult
    = EvalOK Fan
    | EvalTimeout
    | EvalDie Fan
  deriving (Eq, Ord)

data Response
    = RespEval EvalResult
    | RespCall Fan

instance ToNoun EvalResult where
    toNoun = \case
        EvalOK x    -> mkRow ["ok", x]
        EvalTimeout -> "timeout"
        EvalDie x   -> mkRow ["die", x]

instance ToNoun Response where
    toNoun = \case
        RespEval er -> mkRow ["eval", toNoun er]
        RespCall x  -> mkRow ["call", x]

rexShow :: ToNoun a => a -> Text
rexShow x =
    let ?rexColors = Rex.NoColors
    in Loot.ReplExe.renderValue True Nothing (toNoun x)

rexShowAs :: ToNoun a => Text -> a -> Text
rexShowAs name x =
    let ?rexColors = Rex.NoColors
    in Loot.ReplExe.renderValue True (Just $ utf8Nat name) (toNoun x)

responseToVal :: Response -> Fan
responseToVal (RespEval (EvalOK f))  = mkRow [NAT 0, f]
responseToVal (RespEval EvalTimeout) = NAT 1
responseToVal (RespEval (EvalDie f)) = mkRow [NAT 2, f]
responseToVal (RespCall f)           = f

data Request
  = ReqEval Nat Fan (Vector Fan)
  | ReqCall Bool Nat Fan
  | UNKNOWN Fan
  deriving (Show)

valToRequest :: Fan -> Request
valToRequest v@(ROW xs) =
  fromMaybe (UNKNOWN v) $ case toList xs of
    [NAT 0, tout, val, args] ->
      ReqEval <$> fromNoun tout <*> pure val <*> fromNoun args
    [dst@(NAT _), NAT 0, msg] ->
      ReqCall <$> pure False <*> fromNoun dst <*> pure msg
    [dst@(NAT _), NAT 1, msg] ->
      ReqCall <$> pure True <*> fromNoun dst <*> pure msg
    _ -> Nothing
valToRequest v = UNKNOWN v

isDurable :: Request -> Bool
isDurable (ReqEval _ _ _)       = False
isDurable (ReqCall durable _ _) = durable
isDurable (UNKNOWN _)           = False

data PendingAdd = PendingAdd RequestIdx Request
  deriving (Show)

data Completed
  = EvalCancelled Int      -- Frees the callid back to the main thread.
  | EvalCompleted Int EvalResult Nat  -- Returns a result, and the runtime.
  | CallCompleted Int Fan

data Work
  = ApplyResponse RequestIdx Response
  | MachineCommand MachineEvent
  | LogCallback BatchNum

data Machine = MACHINE {
  -- * Logging

  -- | Handle to communicate with the logging system.
  lmdbThread              :: LmdbThread,

  -- | Name of this Machine in the shared lmdbstore.
  machineName             :: CogName,

  -- | The batch number of the next LogBatch.
  nextBatchNum            :: BatchNum,

  -- | The batch number of the last LogBatch which had a snapshot (or 0 if
  -- none).
  lastSnapshot            :: BatchNum,

  -- | Number of microseconds of work performed in `pendingReceipts`.
  workSinceLastBatch      :: Nat,

  -- | Number of microseconds of work performed since the `lastSnapshot`
  -- `LogBtach`.
  workSinceLastSnapshot   :: Nat,

  -- | The state of #noun at the time the current batch started.
  snapshotAtStartOfBatch  :: Fan,

  -- | When Just, the BatchNum of the write job we've asked the
  persistingBatchNum      :: Maybe BatchNum,

  -- | Receipts which are queued to be persisted to disk in the next LogBatch.
  pendingReceipts         :: [Receipt],

  -- | Whether there's a pending add caused by a receipt. We keep track of this
  -- so we write to disk at the first opportunity when blocking.
  pendingAddsInReceipts   :: Bool,

  -- | Pending calls that can only be released after the associated BatchNum is
  -- released after it's persisted.
  pendingAddsByLogBatch   :: IntMap (Seq PendingAdd),

  -- | Pending adds indexed by RequestIdx, for quick canceling.
  pendingAddsByRequestIdx :: Map RequestIdx Int,

  -- ---------------------------------------------------------------------------

  -- * Plunder Execution

  -- | The raw plunder noun of the machine.
  noun                    :: Fan,

  -- | The parsed requests from machine noun.
  requests                :: IntMap (Fan, Request),

  -- ---------------------------------------------------------------------------

  -- * EVAL handling

  -- | Unique per-event instance ids, to not clash with reusable RequestIdxs.
  evalFreelist            :: IntFreelist,

  -- | Mapping from positional idx to persistent
  evalIdxMap              :: Map RequestIdx Int,
  evalIdMap               :: IntMap RequestIdx,

  -- | Evals we've spawned off to go do work.
  evalWaiting             :: Map RequestIdx (Async ()),

  -- ---------------------------------------------------------------------------

  -- * CALL handling
  callIdFreelist          :: IntFreelist,
  callIdToReqIdx          :: IntMap RequestIdx,
  reqIdxToCallId          :: Map RequestIdx Int,
  liveCalls               :: IntMap (TVar (Maybe (Fan -> STM ()))),

  hardwareCall            :: RouterFun,

  -- ---------------------------------------------------------------------------

  -- * Work Queues

  -- | Queue for when a LogBatch has been persisted to disk.
  logCallbackQ            :: TQueue BatchNum,

  -- | Queue of priority machine related events that must be processed before
  -- user work.
  machineEventQ           :: TQueue MachineEvent,

  -- | Queue of incoming completed work. When an eval is done, it places its
  -- work in this q.
  completedQ              :: TQueue Completed
  }

makeFieldLabelsNoPrefix ''Machine

{-
    Returns an empty `Machine` connected to the `RouterFun`
    and `LmdbThread`, but with placeholder data for `#noun`,
    `#requests`, etc.
-}
newMachine
    :: TQueue MachineEvent
    -> RouterFun
    -> LmdbThread
    -> CogName
    -> IO Machine
newMachine mq inRouterFun dbt name = do
  logCallbackQ <- newTQueueIO
  completedQ   <- newTQueueIO
  pure MACHINE
    { lmdbThread              = dbt
    , machineName             = name
    , nextBatchNum            = BatchNum 0
    , lastSnapshot            = BatchNum 0
    , workSinceLastBatch      = 0
    , workSinceLastSnapshot   = 0
    , snapshotAtStartOfBatch  = NAT 0
    , persistingBatchNum      = Nothing
    , pendingReceipts         = []
    , pendingAddsInReceipts   = False
    , pendingAddsByLogBatch   = mempty
    , pendingAddsByRequestIdx = mempty
    , noun                    = NAT 0
    , requests                = mempty
    , evalFreelist            = emptyIntFreelist
    , evalIdxMap              = mempty
    , evalIdMap               = mempty
    , evalWaiting             = mempty
    , callIdFreelist          = emptyIntFreelist
    , callIdToReqIdx          = mempty
    , reqIdxToCallId          = mempty
    , liveCalls               = mempty
    , hardwareCall            = inRouterFun
    , logCallbackQ            = logCallbackQ
    , machineEventQ           = mq
    , completedQ              = completedQ
    }

replayMachine
    :: Debug
    => RouterFun -> LmdbThread -> ReplayFrom -> CogName
    -> IO MachineHandle
replayMachine routerFun dbt replayFrom name = do
    debugText ("REPLAY: " <> name.txt)
    thControlQ <- newTQueueIO
    machine    <- newMachine thControlQ routerFun dbt name
    thAsync    <- async $ flip evalStateT machine $ start
    pure MachineHandle{..}
  where
    start :: StateT Machine IO ()
    start = do
        q <- liftIO $ newTBMQueueIO 100
        liftIO $ atomically $ queueReplayFrom dbt name replayFrom q
        readUntilClosed True q

    readUntilClosed :: Bool -> TBMQueue LogBatch -> StateT Machine IO ()
    readUntilClosed isFirst q = do
      batch <- (liftIO $ atomically $ readTBMQueue q)
      --debugText $ tshow batch
      case batch of
        Nothing -> do
          -- Since we're replaying, any "pending" adds were
          -- already committed to the log and should immediately
          -- by handled

          pending <- parseRequests
          -- debugText $ tshow ("pending"::Text, pending)

          forM_ pending $ \(PendingAdd idx req) -> do
            addRequest (idx, req)

          debugText $ "<< Completed Replay >>"

          mainloop

        Just lb -> do
            replayBatch isFirst lb
            readUntilClosed False q

    replayBatch :: Bool -> LogBatch -> StateT Machine IO ()
    replayBatch True lb =
        -- Ensure we're starting from a place with a valid snapshot.
        case snapshot lb of
            Just ss -> do
                st <- evaluate (force ss)
                assign #noun ss
                assign #snapshotAtStartOfBatch st
                replayBatch False lb
            _ -> do
                error "Invalid LogBatch. Can't resume from here."

    replayBatch False lb@LogBatch{batchNum, executed, snapshot} = do
      debugText $ tshow lb{snapshot=Nothing}
      case lb.snapshot of
          Nothing -> do
              debugText "(NO SNAPSHOT)"
          Just vl -> do
              debugFanVal "snapshot" vl
      assign' #nextBatchNum (1 + batchNum)
      when (isJust snapshot) $ assign' #lastSnapshot batchNum
      for_ executed $ \case
        ReceiptEvalOK idx -> do
          -- When we performed an eval which succeeded the first time.
          getEvalFunAt idx >>= \case
            Nothing -> error "Invalid OK receipt in LogBatch. Can't resume."
            Just (fun, args)  -> do
              let exe s = force (s %% toNoun idx %% foldl' (%%) fun args)
              modifying' #noun exe

        ReceiptVal idx val -> do
          modifying' #noun (\s -> force (s %% (toNoun idx) %% val))

    getEvalFunAt :: RequestIdx -> StateT Machine IO (Maybe (Fan, Vector Fan))
    getEvalFunAt (RequestIdx idx) = do
      row <- getCurrentReqNoun
      case row V.!? idx of
        Nothing -> pure Nothing
        Just val -> do
          case valToRequest val of
            ReqEval _ fun args -> pure $ Just (fun, args)
            _                  -> pure Nothing

mainloop :: Debug => StateT Machine IO ()
mainloop = do
 -- debugText "mainloop"
 getNextWork >>= \case
  LogCallback batch -> do
    debugText "LogCallback"
    assign #persistingBatchNum Nothing
    releasePendingFor batch
    maybeCommitLogBatch
    mainloop
  MachineCommand MachineEventSnapshotAndShutdown -> do
    cogName <- use #machineName
    debugText ("forceSnapshot: " <> cogName.txt)
    forceSnapshot
  MachineCommand MachineEventImmediateShutdown -> do
    debugText "immediate shutdown"
    pure ()
  ApplyResponse idx resp -> do
    debugFan $ mkRow [ "ApplyResponse"
                     , NAT (fromIntegral idx.int)
                     , toNoun resp
                     ]
    mybPendingAdds <- runResponse idx resp
    case mybPendingAdds of
      Nothing -> do
        debugText "Handle runResponse crashing. Queue event for future?"
        error "Handle runResponse crashing. Queue event for future?"
      Just pendingAdds -> do
        addReceipt $ responseToReceipt idx resp
        queuePendingAdds pendingAdds
        when (not $ null pendingAdds) $ assign #pendingAddsInReceipts True
        maybeCommitLogBatch
        mainloop

-- * Logging and Such

-- ** Durable Request Management

-- | For all adds that request durable persistence, queue them so they get run
-- after the next batch gets committed.
queuePendingAdds :: [PendingAdd] -> StateT Machine IO ()
queuePendingAdds pendingAdds = do
  next <- (fromIntegral . unBatchNum) <$> use #nextBatchNum
  forM_ pendingAdds $ \(PendingAdd requestIdx _) ->
    modifying' #pendingAddsByRequestIdx (M.insert requestIdx next)
  modifying' (#pendingAddsByLogBatch % at next % non' _Empty) $ \rablb ->
    foldl' append rablb pendingAdds
  where
    append s a = s Q.|> a

-- | For every add that requested durable persistence, perform the add
-- associated with a BatchNum.
releasePendingFor :: Debug => BatchNum -> StateT Machine IO ()
releasePendingFor (BatchNum nat) = do
  let i = fromIntegral nat
  pending <- use (#pendingAddsByLogBatch % at i % non' _Empty)
  forM_ pending $ \(PendingAdd requestIdx request) -> do
    addRequest (requestIdx, request)
    modifying' #pendingAddsByRequestIdx (deleteMap requestIdx)
  modifying' #pendingAddsByLogBatch (deleteMap i)

-- | If the given RequestIdx might be a pending add, remove stored references
-- to it.
cancelPendingFor :: RequestIdx -> StateT Machine IO ()
cancelPendingFor requestIdx = do
  use #pendingAddsByRequestIdx <&> lookup requestIdx >>= \case
    Nothing -> pure ()
    Just batchNum -> do
      modifying' #pendingAddsByRequestIdx (deleteMap requestIdx)
      modifying' (#pendingAddsByLogBatch % at batchNum % non' _Empty)
                 (del requestIdx)
  where
    del x = Q.filter (\(PendingAdd i _) -> i == x)

-- ** Receipts

responseToReceipt :: RequestIdx -> Response -> Receipt
responseToReceipt idx (RespEval (EvalOK _)) = ReceiptEvalOK idx
responseToReceipt idx resp = ReceiptVal idx (responseToVal resp)

addReceipt :: Receipt -> StateT Machine IO ()
addReceipt receipt = modifying' #pendingReceipts ((:) receipt)

-- | If there isn't an in-flight logbatch write, start one using some heuristic
-- combination of if
maybeCommitLogBatch :: StateT Machine IO ()
maybeCommitLogBatch = do
  curPersisting         <- use #persistingBatchNum
  _pendingReceipts      <- use #pendingReceipts
  pendingAddsInReceipts <- use #pendingAddsInReceipts
  workSinceLastBatch    <- use #workSinceLastBatch
  workSinceLastSnapshot <- use #workSinceLastSnapshot

  -- TODO: Make this conditional more complicated, to consider:
  --
  --  - Is something blocking on this?
  --  - Is there a lot of data pending?
  when (isNothing curPersisting &&
        (pendingAddsInReceipts ||
         workSinceLastBatch > fiveSecondsInUs ||
         workSinceLastSnapshot > twoMinutesInUs)) $ do
    let doSnapshot = workSinceLastSnapshot > twoMinutesInUs
    commitLogBatch doSnapshot

commitLogBatch :: Bool -> StateT Machine IO ()
commitLogBatch doSnapshot = do
  lmdb <- use #lmdbThread
  name <- use #machineName
  lb <- buildLogBatch doSnapshot

  q <- use #logCallbackQ
  let cb = writeTQueue' q (batchNum lb)

  --debugText $ "Beginning commit " ++ tshow lb
  atomically $ queueWriteLogBatch lmdb name lb cb
  assign #persistingBatchNum (Just (batchNum lb))

  assign #pendingAddsInReceipts False
  assign #workSinceLastBatch 0
  when doSnapshot $ assign #workSinceLastSnapshot 0

-- | Pauses and forces a snapshot to disk.
forceSnapshot :: StateT Machine IO ()
forceSnapshot = do
  -- If we're currently waiting for a batch, finish waiting for that batch.
  curPersisting <- use #persistingBatchNum
  logCallbackQ  <- use #logCallbackQ
  when (isJust curPersisting) $ atomically $ do
    readTQueue logCallbackQ
    pure ()

  -- Write the final LogBatch, with its snapshot at the beginning.
  commitLogBatch True
  atomically $ readTQueue logCallbackQ
  pure ()

-- | Creates a log batch in preparation for writing to the datastore.
buildLogBatch :: Bool
              -> StateT Machine IO LogBatch
buildLogBatch makeSnapshot = do
  batchNum <- use #nextBatchNum
  lastSnapshot <- use #lastSnapshot
  snapshot <- case makeSnapshot of
    True -> do
      assign #lastSnapshot batchNum
      ss <- use #snapshotAtStartOfBatch
      pure $ Just ss
    False -> do
      pure Nothing

  use #noun >>= assign #snapshotAtStartOfBatch

  modifying' #nextBatchNum (+ 1)

  receipts <- reverse <$> use #pendingReceipts
  assign #pendingReceipts []
  LogBatch <$> pure batchNum
           <*> getNanoTime
           <*> pure lastSnapshot
           <*> pure snapshot
           <*> pure receipts

data NextType
  = LogBatchWritten BatchNum
  | NextMachineEvent MachineEvent
  | NextCompleted Completed

getNextWork :: Debug => StateT Machine IO Work
getNextWork = do
  use #requests <&> null >>= \case
    True -> do
        debugText "COG STOPPED SPINNING (No more requests)"
        -- finalState <- use #noun
        -- liftIO $ colorsOnlyInTerminal do
            -- Loot.ReplExe.printValue stdout False Nothing finalState
        pure $ MachineCommand MachineEventImmediateShutdown
    False -> do
      logCallbackQ <- use #logCallbackQ
      completedQ <- use #completedQ
      machineQ <- use #machineEventQ
      next <- atomically $
         (LogBatchWritten <$> readTQueue logCallbackQ) <|>
         (NextMachineEvent <$> readTQueue machineQ) <|>
         (NextCompleted <$> readTQueue completedQ)
      case next of
        LogBatchWritten b -> pure $ LogCallback b
        NextMachineEvent ev -> pure $ MachineCommand ev
        NextCompleted c -> do
          handleCompleted c >>= \case
            Nothing -> getNextWork
            Just r  -> pure $ r
  where
    handleCompleted = \case
      EvalCancelled callid -> do
        -- When the cancellation was started from the machine thread, the id
        -- and idx maps were cleaned up. This just safely returns the callid
        -- for reuse.
        zoom #evalFreelist (freelistPush callid)
        pure Nothing

      EvalCompleted callid val runtimeUs -> do
        -- Increase both the per-batch work done and the cumulative snapshot
        -- time.
        modifying' #workSinceLastBatch (+ runtimeUs)
        modifying' #workSinceLastSnapshot (+ runtimeUs)

        zoom #evalFreelist (freelistPush callid)
        use #evalIdMap <&> lookup callid >>= \case
          Nothing -> do
            -- It's not an error to receive a completed message that we don't
            -- have a callid for. This happens when a request is canceled but
            -- the response is already queued.
            pure Nothing
          Just idx -> do
            pure $ Just $ ApplyResponse idx (RespEval val)

      CallCompleted callid val -> do
        zoom #callIdFreelist (freelistPush callid)
        use #callIdToReqIdx <&> lookup callid >>= \case
          Nothing -> do
            -- It's not an error to receive a completed message that we don't
            -- have a callid for. This happens when a request is canceled but
            -- the response is already queued.
            pure Nothing
          Just idx -> do
            pure $ Just $ ApplyResponse idx (RespCall val)

runResponse
    :: Debug
    => RequestIdx
    -> Response
    -> StateT Machine IO (Maybe [PendingAdd])
runResponse reqidx@(RequestIdx key) resp = do
  let respVal = responseToVal resp
  cancelReq key
  noun <- use #noun
  evalResult <- liftIO $
    evalWithTimeout thirtySecondsInUs noun $
    V.fromList [toNoun reqidx, respVal]
  case evalResult of
    -- TODO: In case of errors on this main loop, we should be handling this by
    -- doing something different, like either
    (runtimeUs, EvalTimeout) -> do
      modifying' #workSinceLastSnapshot (+ runtimeUs)
      pure Nothing
    (runtimeUs, EvalDie _) -> do
      modifying' #workSinceLastSnapshot (+ runtimeUs)
      pure Nothing
    (runtimeUs, EvalOK result) -> do
      modifying' #workSinceLastBatch (+ runtimeUs)
      modifying' #workSinceLastSnapshot (+ runtimeUs)
      assign #noun result
      Just <$> parseRequests

parseRequests :: Debug => StateT Machine IO [PendingAdd]
parseRequests = do
    expected <- use #requests
    requests <- getCurrentReqNoun

    {-
    liftIO $ Rex.colorsOnlyInTerminal
           $ Loot.ReplExe.printValue stdout False Nothing
           $ toNoun ( "parseRequests" :: Text
                    , mkRow (fst <$> toList expected)
                    , requests
                    )
    -}

    changedPending <- for (mapToList expected) $ \(i,(v,_)) ->
        case requests V.!? i of
            Nothing       -> do; cancelReq i; pure Nothing
            Just w | v==w -> pure Nothing
            Just w        -> do; cancelReq i; createReq i w

    newPending <- for (zip [0..] (toList requests)) $ \(i,v) ->
        case member i expected of
            False -> createReq i v
            True  -> pure Nothing

    pure $ catMaybes $ changedPending ++ newPending

createReq :: Debug => Int -> Fan -> StateT Machine IO (Maybe PendingAdd)
createReq i v = do
  let req = valToRequest v
  modifying #requests (insertMap i (v, req))
  case isDurable req of
    False -> do
      addRequest (RequestIdx i, req)
      pure Nothing
    True -> do
      pure $ Just $ PendingAdd (RequestIdx i) req

cancelReq :: Int -> StateT Machine IO ()
cancelReq key = do
  Just (_, req) <- use ( #requests % at key )
  modifying #requests (deleteMap key)
  removeRequest (RequestIdx key, req)

getCurrentReqNoun :: StateT Machine IO (Vector Fan)
getCurrentReqNoun = do
    s <- use #noun
    pure $ fromMaybe mempty $ do
        (_x, req) <- getCell s
        getRow req
  where
    getCell :: Fan -> Maybe (Fan, Fan)
    getCell v@KLO{} = Just (boom v)
    getCell _       = Nothing


data EvalCancelledError = EVAL_CANCELLED
  deriving (Exception, Show)

evalWithTimeout :: Nat -> Fan -> Vector Fan -> IO (Nat, EvalResult)
evalWithTimeout ms fun args = do
  startUs <- getNanoTime
  raw <- timeout (fromIntegral ms) $ do
    try (evaluate $ force $ foldl' (%%) fun args) >>= \case
      Left (USER_ERROR p) -> do
        pure $ EvalDie p
      Right f             -> do
        pure $ EvalOK f
  endUs <- getNanoTime
  let runtime = endUs - startUs
  case raw of
    Nothing -> pure (runtime, EvalTimeout)
    Just x  -> pure (runtime, x)

vLive :: TMVar Int
vLive = unsafePerformIO $ newTMVarIO 0

execAsync :: Int -> TQueue Completed -> Nat -> Fan -> Vector Fan
          -> IO (Async ())
execAsync callid q ms fun args = async $ bracket_ start end go
  where
    start = atomically $ do
      liv <- takeTMVar vLive
      when (liv > numCapabilities) retry
      putTMVar vLive (liv+1)

    end = atomically $ do
      liv <- takeTMVar vLive
      putTMVar vLive (liv-1)

    go = do
      try (evalWithTimeout ms fun args) >>= \case
        Left EVAL_CANCELLED ->
          atomically $ writeTQueue' q $ EvalCancelled callid
        Right (timeSpent, x)             -> do
          atomically $ writeTQueue' q $ EvalCompleted callid x timeSpent

addRequest :: Debug => (RequestIdx, Request) -> StateT Machine IO ()

addRequest (idx, ReqEval timeoutMs fun args) = do
  callid <- zoom #evalFreelist freelistGetNext
  completedQ <- use #completedQ
  a <- liftIO $ execAsync callid completedQ (fromIntegral timeoutMs) fun args

  modifying' #evalIdxMap (insertMap idx callid)
  modifying' #evalIdMap (insertMap callid idx)
  modifying' #evalWaiting (insertMap idx a)

addRequest (idx, ReqCall _ dst val) = do
    -- Associate a unique call identifier with this request, since this request
    -- could be canceled and then have the same request id assigned to it.
    callid <- zoom #callIdFreelist freelistGetNext
    completedQ <- use #completedQ
    cb <- newTVarIO $
      Just (\x -> writeTQueue completedQ (CallCompleted callid x))
    modifying #callIdToReqIdx (insertMap callid idx)
    modifying #liveCalls      (insertMap callid cb)
    modifying #reqIdxToCallId (insertMap idx callid)

    name <- use #machineName
    call <- use #hardwareCall
    atomically (call dst name val cb)

-- "Unknown 0" means an empty space.
addRequest (_, UNKNOWN (NAT 0)) = pure ()

addRequest (_, UNKNOWN x) =
  debugText $ "Warning: Unknown request \"" ++ tshow x ++ "\""

removeRequest :: (RequestIdx, Request) -> StateT Machine IO ()

removeRequest (idx, ReqEval _ _ _) = do
  use #evalIdxMap <&> lookup idx >>= \case
    Nothing -> error "No request index in evalIdxMap"
    Just callid -> do
      modifying' #evalIdxMap (deleteMap idx)
      modifying' #evalIdMap (deleteMap callid)

  use #evalWaiting <&> lookup idx >>= \case
    Nothing -> error "No request index in evalWaiting"
    Just a -> do
      modifying' #evalWaiting (deleteMap idx)
      throwTo (asyncThreadId a) EVAL_CANCELLED

  -- TODO: Must clean up from pending add map.

removeRequest (idx, ReqCall _ _ _) = do
  -- Cancel the existing
  use #reqIdxToCallId <&> lookup idx >>= \case
    Nothing -> error "Trying to cancel non-existent request?"
    Just callid -> do
      -- Don't clean up callIdFreelist, that only happens during CallCompleted
      -- and it's better to leak call ids than double use them.

      modifying' #callIdToReqIdx (deleteMap callid)
      modifying' #reqIdxToCallId (deleteMap idx)

      -- TODO: I'm only half sure this is the semantics. If the way to cancel a
      -- response is to NOT set this TVar to Nothing, redo this section.
      use #liveCalls <&> lookup callid >>= \case
        Nothing -> pure ()
        Just v  -> atomically $ writeTVar v Nothing

      modifying' #liveCalls (deleteMap callid)

removeRequest (_, UNKNOWN _) = pure ()

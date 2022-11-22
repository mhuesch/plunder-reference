{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.LmdbStore
    ( LmdbThread(..)
    , LmdbStore(..)
    , openDatastore
    , loadMachine
    , getMachineNames
    , listPins
    , graphPins
    , queueReplayFrom
    , queueWriteLogBatch
    , loadPinByHash
    --
    , tossFanAtCushion
    , grabPinFromCushion
    --
    , close
    , setCursorTo
    )
where

import Server.Debug
import Database.LMDB.Raw
import PlunderPrelude       hiding (log)
import Server.Types.Logging

import Control.Monad.State   (StateT, evalStateT)
import Data.Acquire          (Acquire, ReleaseType(..), mkAcquire,
                              mkAcquireType, with)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr           (Ptr, castPtr, nullPtr)
import Foreign.Storable      (peek, poke, sizeOf)
import Jar                   (DecodeErr, capBS, jar)
import Server.Convert        (fromNoun, toNoun)
import System.Directory      (createDirectoryIfMissing)

import qualified Data.ByteArray  as BA
import qualified Data.ByteString as BS
import qualified Plun            as P

-- -----------------------------------------------------------------------
-- Low level interface
-- -----------------------------------------------------------------------

data LmdbStore = LmdbStore
    { env                   :: !MDB_env

    -- | A map of tables where each table is the numeric kv
    , machineEventlogTables :: !(Map CogName MachineLogTable)

    -- | List of all processes this machine contains.
    , machineTable          :: !MDB_dbi

    -- | A table of machine name to noun representation of a backup
    -- pointer.
    , backupsTable          :: !MDB_dbi

    -- | A mapping of noun hash to serialized noun representation.
    , pinCushion            :: !MDB_dbi
    }

-- | A table mapping a numeric batch number to a noun hash which has the
data MachineLogTable = MachineLogTable
    { logTable   :: !MDB_dbi
    , numBatches :: !(TVar Word64)
    }

instance Show MachineLogTable where
    show logs = unsafePerformIO do
        n <- readTVarIO logs.numBatches
        pure ("MachineLogTable{" <> show n <> "}")

data StoreExn
    = BadMachineName CogName
    | BadWriteMachineTable CogName
    | BadIndexInEventlog CogName
    | BadWriteLogBatch CogName BatchNum
    | BadPinWrite ByteString
    | BadPinMissing ByteString [ByteString]
    | BadDependency ByteString
    | BadDependencyDecode ByteString DecodeErr
    | BadLogEntry CogName
    | EmptyLogTable CogName
  deriving Show

instance Exception StoreExn where

makeFieldLabelsNoPrefix ''LmdbStore

openEnv :: MonadIO m => FilePath -> m MDB_env
openEnv dir = liftIO $ do
    env <- mdb_env_create
    mdb_env_set_maxdbs env 1024 -- TODO: Adjust this dynamically?
    mdb_env_set_mapsize env (1024 ^ (4::Int))
    mdb_env_open env dir []
    pure env

openTables :: MDB_txn -> IO (MDB_dbi, MDB_dbi, MDB_dbi)
openTables txn =
    (,,) <$> mdb_dbi_open txn (Just "MACHINES") [MDB_CREATE]
         <*> mdb_dbi_open txn (Just "BACKUPS") [MDB_CREATE, MDB_INTEGERKEY]
         <*> mdb_dbi_open txn (Just "NOUNS") [MDB_CREATE]

logName :: CogName -> Maybe Text
logName nm = Just ("LOG-" ++ nm.txt)

open :: FilePath -> IO LmdbStore
open dir = do
    createDirectoryIfMissing True dir
    env <- openEnv dir
    with (writeTxn env) $ \txn -> do
        (t, b, n) <- openTables txn
        machineTables <- mapFromList <$> readMachines txn t
        pure $ LmdbStore env machineTables t b n
  where
    readMachines txn s =
      getAllMachines txn s >>= traverse (openMachineTable txn)

    openMachineTable txn tn = do
      tbl  <- mdb_dbi_open txn (unpack <$> logName tn) [MDB_INTEGERKEY]
      num  <- getNumBatches tn txn tbl
      vNum <- newTVarIO (num + 1)
      pure $ (tn, MachineLogTable tbl vNum)

close :: FilePath -> LmdbStore -> IO ()
close _dir db = do
    mdb_dbi_close db.env db.backupsTable
    for_ (mapToList db.machineEventlogTables) \(_,s) ->
        mdb_dbi_close db.env s.logTable
    mdb_dbi_close db.env db.pinCushion
    mdb_env_sync_flush db.env
    mdb_env_close db.env

-- -----------------------------------------------------------------------

getAllMachines :: MDB_txn -> MDB_dbi -> IO [CogName]
getAllMachines txn machineTable =
    with (cursor txn machineTable) \cur ->
        forRange cur RSBegin \pKey _ -> do
            key <- peek pKey >>= valToBS
            pure $ COG_NAME $ decodeUtf8 key

getNumBatches :: CogName -> MDB_txn -> MDB_dbi -> IO Word64
getNumBatches tn txn logTable =
    with (cursor txn logTable) $ \cur ->
        withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr) $ \k v -> do
            mdb_cursor_get MDB_LAST cur k v >>= \case
                False -> pure 0
                True  -> peek k >>= (parseEventLogKey tn)

getAllMachinesInStore :: StateT LmdbStore IO [CogName]
getAllMachinesInStore = keys <$> use #machineEventlogTables

graphAllPinsInStore :: StateT LmdbStore IO [(ByteString, [ByteString])]
graphAllPinsInStore = do
    env <- use #env
    pinCushion <- use #pinCushion
    liftIO $
        with (writeTxn env) \txn ->
        with (cursor txn pinCushion) \cur ->
        forRange cur RSBegin \pKey pVal ->
        do
            key <- peek pKey >>= valToBS
            val <- peek pVal >>= valToBS
            let (deps, _bytz) = deserializeRep val
            pure (key, toList deps)

listAllPinsInStore :: StateT LmdbStore IO [ByteString]
listAllPinsInStore = do
    env <- use #env
    pinCushion <- use #pinCushion
    liftIO $
        with (writeTxn env) \txn ->
        with (cursor txn pinCushion) \cur ->
        forRange cur RSBegin \pKey _ ->
        (peek pKey >>= valToBS)

loadMachineFromStore :: CogName -> StateT LmdbStore IO LoadMachine
loadMachineFromStore machineName@(COG_NAME tnStr) = do
    logTables <- use #machineEventlogTables
    case lookup machineName logTables of
        Nothing   -> startNewMachine
        Just logs -> machineExists logs.numBatches
  where
    flags = compileWriteFlags [MDB_NOOVERWRITE]
    tnBS = encodeUtf8 tnStr

    startNewMachine :: StateT LmdbStore IO LoadMachine
    startNewMachine = do
      env <- use #env
      table <- use #machineTable
      log <- liftIO $ with (writeTxn env) $ \txn -> do
        -- TODO: Attach a value to the key. Empty for now for iteration.
        writeKV flags txn table tnBS BS.empty (BadWriteMachineTable machineName)

        -- Build an empty table with that machine name.
        machineDb <- mdb_dbi_open txn (unpack <$> logName machineName)
                       [MDB_CREATE, MDB_INTEGERKEY]
        numEvents <- newTVarIO 0
        pure $ MachineLogTable machineDb numEvents

      modifying #machineEventlogTables (insertMap machineName log)
      pure NewMachine

    machineExists numBatches = do
      nb <- atomically $ readTVar numBatches
      pure $ ExistingMachine $ BatchNum $ fromIntegral nb

loadLogBatches
    :: CogName -> ReplayFrom -> TBMQueue LogBatch
    -> StateT LmdbStore IO ()
loadLogBatches who rf q = do
    logTables <- use #machineEventlogTables
    env <- use #env
    case lookup who logTables of
        Nothing   -> throwIO (BadMachineName who)
        Just logs -> do
            pinCushion <- use #pinCushion
            liftIO $ with (readTxn env) $ loadBatches logs.logTable pinCushion
  where
    loadBatches log pinCushion txn = do
      case rf of
        EarliestBatch  -> readRangeStarting log pinCushion txn RSBegin
        LatestSnapshot -> with (cursor txn log) $ \cur -> do
          findLastSnapshot pinCushion log txn cur

    readRangeStarting log pinCushion txn firstKey = do
      with (cursor txn log) $ \cur -> do
        void $ forRange cur firstKey \k v -> do
          kBS <- peek k >>= valToBS
          vBS <- peek v >>= valToBS
          val <- loadNoun txn pinCushion kBS vBS
          case fromNoun val of
            Nothing -> error "Couldn't read noun"
            Just lb -> atomically $ writeTBMQueue q lb
        atomically $ closeTBMQueue q

    findLastSnapshot pinCushion log txn cur = do
      withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr) $ \k v -> do
        found <- mdb_cursor_get MDB_LAST cur k v
        unless found $ throwIO (EmptyLogTable who)
        kBS <- peek k >>= valToBS
        vBS <- peek v >>= valToBS
        val <- loadNoun txn pinCushion kBS vBS
        case fromNoun val of
          Nothing -> throwIO (BadLogEntry who)
          Just lb -> case (snapshot lb) of
            Just _ -> do
              -- The last logbatch was a snapshot. Just return it.
              atomically $ do
                writeTBMQueue q lb
                closeTBMQueue q
            Nothing -> do
              readRangeStarting log pinCushion txn $
                RSAt (fromW64 $ fromIntegral $ unBatchNum $ lastSnapshot lb)

writeLogBatch :: CogName -> LogBatch -> StateT LmdbStore IO ()
writeLogBatch who lb = do
    logTables <- use #machineEventlogTables
    case lookup who logTables of
        Nothing -> throwIO (BadMachineName who)
        Just logs -> do
            (pinz, lbNoun) <- liftIO (jar $ toNoun lb)
            let lbHash = P.cryptographicIdentity pinz lbNoun
            batch <- atomically (readTVar logs.numBatches)
            let next = batch + 1
            writeIt batch logs.logTable lbHash lbNoun pinz
            atomically $ writeTVar logs.numBatches next
  where
    logFlags = compileWriteFlags [MDB_NOOVERWRITE]

    -- "it" being the current batch.
    writeIt curBatch logTable _lbHash lbNoun pinz = do
        when (curBatch /= (fromIntegral $ unBatchNum lb.batchNum)) $
            error ( "Database/Runtime batch number mismatch: database expects "
                 ++ show curBatch ++ " but received "
                 ++ show (unBatchNum lb.batchNum)
                  )

        let rep = concat $ serializeRep pinz lbNoun

        env <- use #env
        pinCushion <- use #pinCushion
        liftIO $ with (writeTxn env) $ \txn -> do
            for_ pinz (stickPinInCushion logFlags txn pinCushion)
            let key :: ByteString = (fromW64 curBatch)
            writeKV logFlags txn logTable key rep
                (BadWriteLogBatch who lb.batchNum)

stickPinInCushion :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> P.Pin -> IO ()
stickPinInCushion logFlags txn pinCushion p = do
    -- Here, we only want to write a pin if the pin isn't already stuck in
    -- the pin cushion.
    alreadyInCushion <- keyExists txn pinCushion p.pinHash
    unless alreadyInCushion do
        for_ p.pinRefs (stickPinInCushion logFlags txn pinCushion)

        let rep = concat (serializeRep p.pinRefs p.pinBlob)
        writeKVNoOverwrite logFlags txn pinCushion p.pinHash
                           rep
                           (BadPinWrite p.pinHash)

coerceIntoPin :: P.Fan -> P.Pin
coerceIntoPin (P.PIN p) = p
coerceIntoPin fan       = coerceIntoPin (P.mkPin fan)

-- | Given a name, find a pin in the pin cushion.
loadPin :: MDB_txn -> MDB_dbi -> ByteString -> IO P.Pin
loadPin txn pinCushion =
    loop []
  where
    loop :: [ByteString] -> ByteString -> IO P.Pin
    loop !stack pinHashBS =
        getKey txn pinCushion pinHashBS >>= \case
            Nothing -> do
                throwIO (BadPinMissing pinHashBS stack)
            Just val -> do
                let (deps, bytz) = deserializeRep val
                depPins <- for deps (\d -> loop (pinHashBS : stack) d)
                case capBS (P.PIN <$> depPins) bytz of
                    Left err   -> throwIO (BadDependencyDecode pinHashBS err)
                    Right valu -> do
                        pin <- P.mkPin' valu
                        evaluate (force pin)

-- | Given a bytestring representing a serialized rep of a noun, load
-- dependencies from the pincushion and return the Val.
loadNoun :: MDB_txn -> MDB_dbi -> ByteString -> ByteString -> IO P.Fan
loadNoun txn pinCushion kBS vBS = do
    let (deps, bytz) = deserializeRep vBS
    refs <- forM deps (fmap P.PIN . loadPin txn pinCushion)
    case capBS refs bytz of
        Left err   -> throwIO (BadDependencyDecode kBS err)
        Right valu -> evaluate (force valu)

serializeRep :: Vector P.Pin -> ByteString -> [ByteString]
serializeRep refs body = toList
    (fmap P.pinHash refs) <> [replicate 32 0, body]

deserializeRep :: ByteString -> (Vector ByteString, ByteString)
deserializeRep = loop []
  where
    loop xs val =
        let (key, rest) = splitAt 32 val in
        if | key == replicate 32 0 -> (fromList $ reverse xs, rest)
           | null key              -> error "Malformed rep in deserializeRep"
           | otherwise             -> loop (key:xs) rest

-- -----------------------------------------------------------------------

readTxn :: MDB_env -> Acquire MDB_txn
readTxn env = mkAcquire (mdb_txn_begin env Nothing True) mdb_txn_abort

writeTxn :: MDB_env -> Acquire MDB_txn
writeTxn env = mkAcquireType begin end
  where
    begin = mdb_txn_begin env Nothing False
    end txn = \case
        ReleaseException -> mdb_txn_abort  txn
        _                -> mdb_txn_commit txn

cursor :: MDB_txn -> MDB_dbi -> Acquire MDB_cursor
cursor txn dbi = mkAcquire (mdb_cursor_open txn dbi) mdb_cursor_close

byteArrayAsMdbVal :: BA.ByteArrayAccess ba => ba -> (MDB_val -> IO a) -> IO a
byteArrayAsMdbVal ba fun =
    BA.withByteArray ba $ \ptr ->
        fun (MDB_val (fromIntegral (BA.length ba)) (castPtr ptr))

keyExists :: BA.ByteArrayAccess a => MDB_txn -> MDB_dbi -> a -> IO Bool
keyExists txn db key =
    byteArrayAsMdbVal key $ \mKey ->
        isJust <$> mdb_get txn db mKey

getKey
    :: BA.ByteArrayAccess a
    => MDB_txn -> MDB_dbi -> a -> IO (Maybe ByteString)
getKey txn db key = byteArrayAsMdbVal key $ \mKey ->
    mdb_get txn db mKey >>= traverse valToBS

writeKV
    :: (BA.ByteArrayAccess a, BA.ByteArrayAccess b)
    => MDB_WriteFlags -> MDB_txn -> MDB_dbi
    -> a
    -> b
    -> StoreExn
    -> IO ()
writeKV flags txn db k v exn = do
    byteArrayAsMdbVal k $ \mKey ->
        byteArrayAsMdbVal v $ \mVal ->
            mdb_put flags txn db mKey mVal >>= \case
                True  -> pure ()
                False -> throwIO exn

-- | Like writeKV, but don't throw an exception or
writeKVNoOverwrite
    :: (BA.ByteArrayAccess a, BA.ByteArrayAccess b)
    => MDB_WriteFlags -> MDB_txn -> MDB_dbi
    -> a
    -> b
    -> StoreExn
    -> IO ()
writeKVNoOverwrite flags txn db k v exn = do
    (keyExists txn db k) >>= \case
        True  -> pure ()
        False -> writeKV flags txn db k v exn

data RangeStart a
    = RSBegin
    | RSAt ByteString

-- | Given a numeric key range starting at (the beginning/a specific number),
-- iterate until the end, passing each kv pair to an action.
forRange
    :: MDB_cursor
    -> RangeStart a
    -> (Ptr MDB_val -> Ptr MDB_val -> IO b)
    -> IO [b]
forRange topCur rs cb = do
    case rs of
        RSBegin -> do
            withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr) $ \k v -> do
                mdb_cursor_get MDB_FIRST topCur k v >>= \case
                    False -> pure []
                    True -> do
                        x <- cb k v
                        continue topCur k v [x]
        RSAt start ->
            withContentsOf start $ \k ->
                withNullPtr $ \v ->
                    mdb_cursor_get MDB_SET_KEY topCur k v >>= \case
                        False -> pure []
                        True -> do
                            x <- cb k v
                            continue topCur k v [x]
  where
      continue cur k v xs = do
          mdb_cursor_get MDB_NEXT cur k v >>= \case
              False -> pure $ reverse xs
              True -> do
                  x <- cb k v
                  continue cur k v (x:xs)

setCursorTo :: MDB_cursor -> MDB_cursor_op -> StoreExn -> IO ()
setCursorTo cur op exn = do
    withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr) $ \k v -> do
        mdb_cursor_get op cur k v >>= \case
            False -> throwIO exn
            True  -> pure ()

valToBS :: MDB_val -> IO ByteString
valToBS (MDB_val sz ptr) = BS.packCStringLen (castPtr ptr, fromIntegral sz)

parseEventLogKey :: CogName -> MDB_val -> IO Word64
parseEventLogKey tn (MDB_val sz ptr) =
    if sz == 8
    then peek (castPtr ptr)
    else throwIO (BadIndexInEventlog tn)

fromW64 :: (BA.ByteArray ba) => Word64 -> ba
fromW64 n = BA.allocAndFreeze 8 $ \p -> poke p n

withContentsOf :: BA.ByteArrayAccess ba => ba -> (Ptr MDB_val -> IO a) -> IO a
withContentsOf ba fn =
    BA.withByteArray ba $ \ptr -> do
        let item = MDB_val (fromIntegral (BA.length ba)) (castPtr ptr)
        allocaBytes (sizeOf item) $ \pK -> do
            poke pK item
            fn pK

withNullPtr :: (Ptr MDB_val -> IO a) -> IO a
withNullPtr fn = do
    let nul = MDB_val 0 nullPtr
    allocaBytes (sizeOf nul) $ \pK -> do
        poke pK nul
        fn pK


-- -----------------------------------------------------------------------
-- User level interaction
-- -----------------------------------------------------------------------

data LmdbThread = LmdbThread
    { events :: TQueue LoggingEvent
    , thread :: Async ()
    }

data LoggingEvent
    = LogGetAllMachines ([CogName] -> STM ())

    | LogListPins ([ByteString] -> STM ())

    | LogGraphPins ([(ByteString, [ByteString])] -> STM ())

    -- | Tries loading a machine. If the machine doesn't exist on disk,
    -- allocates structures and returns NewMachine, otherwise returns
    -- the latest BatchNum.
    | LogLoadMachine CogName (LoadMachine -> STM ())

    -- | Read and stream logbatches into the passed in queue. Closes the
    -- queue when done.
    | LogReplayFrom CogName ReplayFrom (TBMQueue LogBatch)

    -- | Writes a logbatch for a specific machine.
    | LogWriteBatch CogName LogBatch (STM ())

    -- | Writes a logbatch for a specific machine.
    | LogFetchPin ByteString (Maybe P.Pin -> STM ())

    -- | Exits the logging thread.
    | LogShutdown

openDatastore :: Debug => FilePath -> Acquire LmdbThread
openDatastore dir =
    mkAcquire start stop
  where
    start = do
        q <- newTQueueIO
        a <- asyncBound $ writerThread q (open dir)
        pure (LmdbThread q a)

    stop (LmdbThread q a) = do
        atomically $ writeTQueue' q LogShutdown
        wait a

lmdbRequest :: LmdbThread -> ((a -> STM ()) -> LoggingEvent) -> IO a
lmdbRequest dbt op = do
    v <- atomically do
        v <- newEmptyTMVar
        writeTQueue' dbt.events $ op (putTMVar v)
        pure v
    atomically (takeTMVar v)

getMachineNames :: LmdbThread -> IO [CogName]
getMachineNames t = lmdbRequest t LogGetAllMachines

listPins :: LmdbThread -> IO [ByteString]
listPins t = lmdbRequest t LogListPins

graphPins :: LmdbThread -> IO [(ByteString, [ByteString])]
graphPins t = lmdbRequest t LogGraphPins

loadPinByHash :: LmdbThread -> ByteString -> IO (Maybe P.Pin)
loadPinByHash t bs = lmdbRequest t (LogFetchPin bs)

loadMachine :: LmdbThread -> CogName -> IO LoadMachine
loadMachine t nm = lmdbRequest t (LogLoadMachine nm)

queueReplayFrom
    :: LmdbThread -> CogName -> ReplayFrom -> TBMQueue LogBatch
    -> STM ()
queueReplayFrom dbt tn rf q =
    writeTQueue' dbt.events (LogReplayFrom tn rf q)

queueWriteLogBatch :: LmdbThread -> CogName -> LogBatch -> STM () -> STM ()
queueWriteLogBatch dbt tn lb cb =
    writeTQueue' dbt.events (LogWriteBatch tn lb cb)

writerThread :: Debug => TQueue LoggingEvent -> IO LmdbStore -> IO ()
writerThread dbWriteEvent mkDbStore =
    handle onErr do
        db <- mkDbStore
        evalStateT loop db
  where
    onErr e = do
        debugText $ "db thread was killed by: " <> pack (displayException e)
        throwIO (e :: SomeException)

    loop = atomically (readTQueue dbWriteEvent) >>= \case
        LogGetAllMachines cb -> do
            machines <- getAllMachinesInStore
            atomically (cb machines)
            loop

        LogListPins cb -> do
            pins <- listAllPinsInStore
            atomically (cb pins)
            loop

        LogGraphPins cb -> do
            table <- graphAllPinsInStore
            atomically (cb table)
            loop

        LogLoadMachine machine cb -> do
            machineState <- loadMachineFromStore machine
            atomically (cb machineState)
            loop

        LogReplayFrom machine rf q -> do
            loadLogBatches machine rf q
            loop

        LogFetchPin hashVal k -> do
            env <- use #env
            dbi <- use #pinCushion
            res <- liftIO do
                with (readTxn env) \txn -> do
                    keyExists txn dbi hashVal >>= \case
                        False -> pure Nothing
                        True  -> Just <$> loadPin txn dbi hashVal
            atomically (k res)
            loop

        LogWriteBatch machine b cb -> do
            writeLogBatch machine b
            atomically cb
            loop

        LogShutdown -> pure ()

--------------------------------------------------------------------------------

{-|
    This is intended to be called from an outside process, to inject a
    plunder value directly into the pin cushion.
-}
tossFanAtCushion :: FilePath -> P.Fan -> IO ByteString
tossFanAtCushion pax val = do
    res <- asyncBound do
        env <- openEnv pax
        pin <- evaluate (coerceIntoPin val)
        dbi <- with (writeTxn env) \txn -> do
            mdb_dbi_open txn (Just "NOUNS") [MDB_CREATE]
        with (writeTxn env) \txn -> do
            stickPinInCushion logFlags txn dbi pin
        mdb_dbi_close env dbi
        mdb_env_sync_flush env
        mdb_env_close env
        pure pin.pinHash
    wait res
  where
    logFlags = compileWriteFlags [MDB_NOOVERWRITE]

-- | This all an outside process grab a pin from the database.
--  plunder value directly into the pin cushion.
grabPinFromCushion :: FilePath -> ByteString -> IO (Maybe P.Pin)
grabPinFromCushion pax key = do
    res <- asyncBound do
        env <- openEnv pax
        dbi <- with (writeTxn env) \txn -> do
                   mdb_dbi_open txn (Just "NOUNS") [MDB_CREATE]
        res <- with (readTxn env) \txn -> do
                   keyExists txn dbi key >>= \case
                       False -> pure Nothing
                       True  -> Just <$> loadPin txn dbi key
        mdb_dbi_close env dbi
        mdb_env_sync_flush env
        mdb_env_close env
        pure res
    wait res

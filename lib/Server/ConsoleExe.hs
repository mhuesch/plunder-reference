{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.ConsoleExe (main) where

import Data.Acquire
import Options.Applicative
import PlunderPrelude       hiding (Handler, handle)
import Servant
import Servant.API.Generic
import Servant.Client
import Server.Hardware
import Server.Types.Logging
import Server.Types.Machine
import System.Environment
import System.Posix.Signals hiding (Handler)
import System.Process
import Server.Debug

import Control.Concurrent    (threadDelay)
import Control.Exception     (handle)
import Control.Monad.Fail    (fail)
import GHC.IO.Exception      (ExitCode)
import Network.HTTP.Client   (defaultManagerSettings, newManager)
import Plun                  (Fan)
import Plun.Print            (decodeBtc, encodeBtc)
import Server.EvalMachine    (replayMachine)
import Server.Hardware.Types (HardwareDb)
import Server.Time           (getNanoTime)
import System.Directory      (createDirectoryIfMissing, doesFileExist,
                              getHomeDirectory, removeFile)
import System.Exit           (ExitCode(..), exitWith)
import System.IO.Error       (catchIOError)
import System.Posix.Types    (CPid(CPid))


import qualified Loot.ReplExe
import qualified Rex
import qualified Sire.ReplExe

import qualified Data.Aeson               as A
import qualified Network.Socket           as N
import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W
import qualified Plun                     as P
import qualified Server.LmdbStore         as DB

--------------------------------------------------------------------------------

safeDeleteFile :: FilePath -> IO ()
safeDeleteFile pax = catchIOError (removeFile pax) (const $ pure ())

getPidFile :: Debug => FilePath -> Acquire FilePath
getPidFile storeDir =
    mkAcquire start safeDeleteFile
  where
    start = do
        createDirectoryIfMissing True storeDir
        pid <- getCurrentPid
        let pax = (storeDir </> "pid.lock")
        debugVal "pidfile" (pack pax :: Text)
        exists <- doesFileExist pax
        when exists $ do
           debugFan "pidfile_exists"
           pidTxt <- readFileUtf8 pax
           case readMay pidTxt of
               Nothing -> do
                   debugFan "Malformed pidfile in store, existing"
                   exitWith (ExitFailure 1)
               Just alien -> do
                   debugFan "Existing daemon is running, killing it"
                   -- TODO Handle situation where process does not
                   -- actually exist.
                   signalProcess sigTERM (alien :: CPid)
                   let loop =
                           doesFileExist pax >>= \case
                               True  -> threadDelay 10_000 >> loop
                               False -> pure ()
                   debugFan "Waiting for it to shut down"
                   loop
                   debugFan "It is down"
        debugFan "write_pidfile"
        writeFileUtf8 pax (tshow (coerce pid :: Int32))
        pure pax

runControlServer :: Debug => ServerState -> Acquire (Async ())
runControlServer st = do
    (_, port, sock) <- mkAcquire openPort (safeDeleteFile . view _1)
    mkAcquire (async $ ctrlServer port sock) cancel
  where
    openPort = do
        -- TODO Only accept input from localhost
        let localhost = N.tupleToHostAddress (0x7f, 0, 0, 1)
        let flags = [N.AI_NUMERICHOST, N.AI_NUMERICSERV]
        let tcp   = 6
        let addr  = (N.SockAddrInet 0 localhost)
        let ainfo = N.AddrInfo flags N.AF_INET N.Stream tcp addr Nothing
        listenSocket <- N.openSocket ainfo
        N.listen listenSocket 5 -- TODO Should this be 5?
        listenPort <- fromIntegral <$> N.socketPort listenSocket

        debugVal "CONTROL_PORT" (tshow listenPort)

        let portsFile = st.storeDir </> "ctl.port"

        debugVal "HTTP_PORT_FILE" (pack @Text portsFile)

        writeFileUtf8 portsFile (tshow listenPort)

        pure (portsFile, listenPort, listenSocket)

    ctrlServer port sock = do
        let set = W.defaultSettings & W.setPort port
        W.runSettingsSocket set sock (pathLogged (ctlServer st))

pathLogged :: Debug => W.Application -> W.Application
pathLogged app r k = do
    debug (r.requestMethod, r.rawPathInfo)
    app r k

data FailedToStartDaemon = FAILED_TO_START_DAEMON
    { lastErr :: SomeException
    , cmd     :: String
    , args    :: [String]
    }
  deriving (Show, Exception)

ensureServer :: Debug => FilePath -> IO (Bool, W.Port)
ensureServer d = do
    createDirectoryIfMissing True d

    let portFile = d </> "ctl.port"

    let logFile = d </> "logs"

    -- TODO Handle situation where file exists but daemon is not actually
    -- running.
    hadToStart <- do
        doesFileExist portFile >>= \case
            True -> do
                debugText "daemon_running"
                pure False
            False -> do
                p <- getExecutablePath
                debugVal "logFile" (pack @Text logFile)
                logH <- openFile logFile WriteMode
                let cmd = "nohup"
                let args = [p, "server", d]
                debugVal "daemonCmd" $ map (pack @Text) ([cmd] <> args)
                void (shellBg cmd args (logH, logH))
                loop Nothing cmd args (0::Int)
                pure True

    txt <- readFileUtf8 portFile
    readMay txt & \case
        Nothing -> throwIO (BAD_PORTS_FILE "daemon" portFile txt)
        Just pt -> pure (hadToStart, pt)
  where
    loop mErr cmd args i =
        case mErr of
            Just err | i>= 1000 ->
                throwIO (FAILED_TO_START_DAEMON err cmd args)
            _ ->
                flip handle (clientRequest d reqIsUp) \e -> do
                    threadDelay 10_000
                    loop (Just e) cmd args (i+1)

--------------------------------------------------------------------------------

data CogStatus
    = IDLE
    | CLOGGED
    | SPINNING
    | STARTING
  deriving (Show, Read)

instance A.ToJSON CogStatus where
  toJSON = A.toJSON . toLower . show

instance A.FromJSON CogStatus where
    parseJSON x = do
       txt <- A.parseJSON x
       let die = fail ("invalid cog status: " <> txt)
       maybe die pure $ readMay (toUpper txt)

type Cog = Capture "cog" CogName

type GET  = Get '[JSON]
type POST = Post '[JSON]


type PinHash = Capture "hash" Text

data CtlApi a = CTL_API
    { isUp   :: a :- "up"                      :> GET ()
    , halt   :: a :- "halt"                    :> POST ()
    , pins   :: a :- "pins"                    :> GET (Map Text [Text])
    , cogs   :: a :- "cogs"                    :> GET (Map CogName CogStatus)
    , spin   :: a :- "cogs" :> Cog :> "spin"   :> POST ()
    , replay :: a :- "cogs" :> Cog :> "replay" :> POST ()
    , boot   :: a :- "boot" :> Cog :> PinHash  :> POST ()
    }
  deriving (Generic)

ctlServer :: Debug => ServerState -> W.Application
ctlServer st =
    serve (Proxy :: Proxy (NamedRoutes CtlApi)) CTL_API{..}
  where
    isUp :: Handler ()
    isUp = pure ()

    pins :: Handler (Map Text [Text])
    pins = do
        tab <- liftIO (DB.graphPins st.lmdb)
        pure $ mapFromList
             $ fmap (\(k,vs) -> (encodeBtc k, encodeBtc <$> vs))
             $ tab

    boot :: CogName -> Text -> Handler ()
    boot n h = liftIO (doBoot st n h)

    {-|
        If we were to respond, there would be a race condition between the
        HTTP response completing and process shutting down (and thereby
        killing this thread) The `forever` just causes the request to
        never respond.  Instead the server will always simply terminate
        the connection with no response.
    -}
    halt :: Handler ()
    halt = forever (putMVar st.termSignal ())

    cogs :: Handler (Map CogName CogStatus)
    cogs = do
        cgz <- liftIO (DB.getMachineNames st.lmdb)
        let tab = mapFromList (cgz <&> \c -> (c, c))
        liftIO $ for tab (getCogStatus >=> maybe (error "impossible") pure)

    spin :: CogName -> Handler ()
    spin cog = do
        void $ liftIO $ spinCog st LatestSnapshot $ COG_NAME cog.txt

    replay :: CogName -> Handler ()
    replay cog = do
        void $ liftIO $ spinCog st EarliestBatch $ COG_NAME cog.txt

    getCogStatus :: CogName -> IO (Maybe CogStatus)
    getCogStatus cog = do
        cgz <- liftIO (setFromList <$> DB.getMachineNames st.lmdb)
        let mach = COG_NAME cog.txt
        if not (member mach (cgz :: Set CogName)) then
            pure Nothing
        else atomically do
            (lookup mach <$> readTVar st.cogHandles) >>= \case
                Nothing -> pure (Just IDLE)
                Just vc -> readTVar vc >>= \case
                   Nothing -> pure (Just STARTING)
                   Just{}  -> pure (Just SPINNING)

reqIsUp   :: ClientM ()
reqBoot   :: CogName -> Text -> ClientM ()
_reqHalt   :: ClientM ()
reqCogs   :: ClientM (Map CogName CogStatus)
reqSpin   :: CogName -> ClientM ()
_reqReplay :: CogName -> ClientM ()

CTL_API { isUp   = reqIsUp
        , boot   = reqBoot
        , halt   = _reqHalt
        , cogs   = reqCogs
        , spin   = reqSpin
        , replay = _reqReplay
        } = client (Proxy @(NamedRoutes CtlApi))

--------------------------------------------------------------------------------

data RunType
    = RTSire FilePath [FilePath]
    | RTLoot FilePath [FilePath]
    | RTBoot FilePath CogName Text
    | RTServ FilePath
    | RTDamn FilePath DaemonAction
    | RTOpen FilePath CogName
    | RTTerm FilePath CogName
    | RTCogs FilePath
    | RTStat FilePath
    | RTSpin FilePath ReplayFrom (Maybe CogName)

cogNameArg :: Parser CogName
cogNameArg = COG_NAME <$> strArgument (metavar "NAME" <> help helpTxt)
  where
    helpTxt = "A name for the cog"

replayFromOption :: Parser ReplayFrom
replayFromOption =
    flag LatestSnapshot EarliestBatch
        ( long "replay-all"
       <> help "Replay log from beginning."
        )

bootHashArg :: Parser Text
bootHashArg = strArgument
    ( metavar "HASH"
   <> help "Boot using this sire file (or pin hash)"
    )

sireFile :: Parser FilePath
sireFile =
    strArgument (metavar "SIRE" <> help helpTxt)
  where
    helpTxt = "A sire file to load before launching the REPL"

lootFile :: Parser FilePath
lootFile =
    strArgument (metavar "LOOT" <> help helpTxt)
  where
    helpTxt = "A loot file to load before starting the REPL"

plunderCmd :: String -> String -> Parser a -> Mod CommandFields a
plunderCmd cmd desc parser =
    command cmd (info (parser <**> helper) (progDesc desc))

runType :: FilePath -> Parser RunType
runType defaultDir = subparser
    ( plunderCmd "term" "Connect to the terminal of a cog."
      (RTTerm <$> storeOpt
              <*> cogNameArg)

   <> plunderCmd "open" "Open a terminal's GUI interface."
      (RTOpen <$> storeOpt
              <*> cogNameArg)

   <> plunderCmd "sire" "Runs an standalone loot repl."
      (RTSire <$> storeOpt
              <*> many sireFile)

   <> plunderCmd "cogs" "List cogs in machine."
      (RTCogs <$> (storeArg <|> storeOpt))

   <> plunderCmd "status" "List cogs in machine with their status."
      (RTStat <$> (storeArg <|> storeOpt))

   <> plunderCmd "spin" "Resume an idle cog."
      (RTSpin <$> storeOpt
              <*> replayFromOption
              <*> fmap Just cogNameArg)

   <> plunderCmd "spin-all" "Resume all idle cogs"
      (RTSpin <$> storeOpt
              <*> replayFromOption
              <*> pure Nothing)

   <> plunderCmd "loot" "Runs an standalone sire repl."
      (RTLoot <$> storeOpt <*> many lootFile)

   <> plunderCmd "boot" "Boots a machine."
      (RTBoot <$> storeOpt
              <*> cogNameArg
              <*> bootHashArg)

   <> plunderCmd "server" "Replays the events in a machine."
        (RTServ <$> (storeArg <|> storeOpt))

   <> plunderCmd "daemon" "Run a daemon (if not already running)."
        (RTDamn <$> (storeArg <|> storeOpt) <*> daemonAction)
    )
  where
    storeHlp = help "Location of plunder data"
    storeArg = strArgument (metavar "STORE" <> storeHlp)
    storeOpt = strOption ( long "store"
                         <> value defaultDir
                         <> short 'd'
                         <> metavar "STORE"
                         <> storeHlp
                          )

data DaemonAction = START | STOP | RESTART

daemonAction :: Parser DaemonAction
daemonAction
    = flag' START   (long "start")
  <|> flag' STOP    (long "stop")
  <|> flag' RESTART (long "restart")

runInfo :: FilePath -> ParserInfo RunType
runInfo defaultDir =
    info (runType defaultDir <**> helper)
        ( fullDesc
       <> progDesc "Let's run plunder."
       <> header "new-network - a test for running plunder machines"
        )

data BadPortsFile = BAD_PORTS_FILE Text FilePath Text
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

data SpinDuringShutdown = SPIN_DURING_SHUTDOWN CogName
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

data CogAlreadySpinning = COG_ALREADY_SPINNING CogName
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

data NoSuchCog = NO_SUCH_COG CogName
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

withDaemon :: Debug => FilePath -> ClientM a -> IO a
withDaemon storeDir act = do
    (_, port) <- ensureServer storeDir

    manager <- newManager defaultManagerSettings

    let baseUrlScheme = Http
    let baseUrlHost   = "localhost"
    let baseUrlPort   = port
    let baseUrlPath   = ""
    either throwIO pure =<< runClientM act (mkClientEnv manager BaseUrl{..})

clientRequest :: FilePath -> ClientM a -> IO a
clientRequest storeDir act = do
    portTxt <- readFileUtf8 (storeDir </> "ctl.port")
    Just (port::Int) <- pure (readMay portTxt)

    manager <- newManager defaultManagerSettings

    let baseUrlScheme = Http
    let baseUrlHost   = "localhost"
    let baseUrlPort   = port
    let baseUrlPath   = ""
    either throwIO pure =<< runClientM act (mkClientEnv manager BaseUrl{..})

-- | Initial test here. We create a store, create one machine in it, and then
-- write one artificial logbatch, and then read it back.
main :: IO ()
main = Rex.colorsOnlyInTerminal do
    home <- getHomeDirectory
    ddir <- lookupEnv "PLUNDER_DIR" <&> maybe (home </> ".plunder") id
    args <- customExecParser
            (prefs (showHelpOnError <> showHelpOnEmpty <> noBacktrack))
            (runInfo ddir)

    withDebugOutput $ case args of
        RTLoot d flz     -> withDaemon d $ liftIO $ Loot.ReplExe.replMain flz
        RTSire d flz     -> withDaemon d $ liftIO $ Sire.ReplExe.replMain d flz
        RTOpen d cog     -> void (openBrowser d cog)
        RTTerm d cog     -> void (openTerminal d cog)
        RTCogs d         -> listCogs d False
        RTStat d         -> listCogs d True
        RTSpin d r m     -> maybe (spinAll d r) (spinOne d r) m
        RTBoot d x y     -> bootCog d x y
        RTServ d         -> runServer d
        RTDamn d START   -> startDaemon d
        RTDamn d STOP    -> killDaemon d
        RTDamn d RESTART -> killDaemon d >> startDaemon d

startDaemon :: Debug => FilePath -> IO ()
startDaemon d =
     ensureServer d >>= \case
         (True,  _) -> debugText "Daemon started."
         (False, _) -> debugText "Daemon already running."

bootCog :: (Debug, Rex.RexColor) => FilePath -> CogName -> Text -> IO ()
bootCog d c pash = do
    --  TODO Check if hash ends with ".sire".
    --  If it does, call Sire.loadFile
    --      Shove the resulting fan into the pin cushion.
    --      Get the hash.
    --      Use that hash instead.
    withDaemon d $
        if (".sire" == drop (length pash - 5) pash)
        then do
            let fil = unpack pash
            e <- liftIO (doesFileExist fil)
            unless e (error $ unpack ("File does not exist: " <> pash))
            mVl <- liftIO (Sire.ReplExe.loadFile d fil)
            val <- case mVl of
                      Nothing -> error $ unpack ( "No value at end of file : "
                                               <> pash
                                                )
                      Just vl -> pure vl
            newHash <- liftIO (DB.tossFanAtCushion d val)
            reqBoot c (encodeBtc newHash)
        else do
            reqBoot c pash

killDaemon :: Debug => FilePath -> IO ()
killDaemon d = do
    let pax = (d </> "pid.lock")
    exists <- doesFileExist pax
    when exists $ do
       pidTxt <- readFileUtf8 pax
       mPid   <- pure (readMay pidTxt)
       whenJust mPid \alien -> do
           debugText "Killing Daemon"
           -- TODO Handle situation where process does not actually exist.
           signalProcess sigTERM (alien :: CPid)

-- TODO Spin request should include ReplayFrom info.
spinOne :: Debug => FilePath -> ReplayFrom -> CogName -> IO ()
spinOne d _r cog =
    withDaemon d (reqSpin cog)

-- TODO Spin request should include ReplayFrom info.
spinAll :: Debug => FilePath -> ReplayFrom -> IO ()
spinAll d _r = do
    withDaemon d do
        status <-  reqCogs
        for_ (keys status) \cog -> do
            reqSpin cog

shellFg :: String -> [String] -> IO ExitCode
shellFg c a = do
    let p = (proc c a) { std_in        = Inherit
                       , std_out       = Inherit
                       , std_err       = Inherit
                       , close_fds     = True
                       , delegate_ctlc = True
                       }
    (_, _, _, ph) <- createProcess p
    waitForProcess ph

shellBg :: String -> [String] -> (Handle, Handle) -> IO ()
shellBg c a (out, err) = do
    let p = (proc c a) { std_in        = NoStream
                       , std_out       = UseHandle out
                       , std_err       = UseHandle err
                       , close_fds     = True
                       , delegate_ctlc = False
                       }
    (_, _, _, _) <- createProcess p
    pure ()

openBrowser :: FilePath -> CogName -> IO ExitCode
openBrowser dir cogNm = do
    let pax = (dir </> unpack (cogNm.txt <> ".http.port"))
    exists <- doesFileExist pax
    unless exists (error "Cog does not serve HTTP")
    port <- do cont <- readFileUtf8 pax
               case readMay @Text @Word cont of
                   Nothing -> throwIO (BAD_PORTS_FILE "http" pax cont)
                   Just pt -> pure pt
    let url = "http://localhost:" <> show port
    shellFg "xdg-open" [url]

openTerminal :: FilePath -> CogName -> IO ExitCode
openTerminal dir cogNm = do
    let pax = (dir </> unpack (cogNm.txt <> ".telnet.port"))
    exists <- doesFileExist pax
    unless exists (error "Cog does not serve Telnet")
    port <- do cont <- readFileUtf8 pax
               case readMay @Text @Word cont of
                   Nothing -> throwIO (BAD_PORTS_FILE "telnet" pax cont)
                   Just pt -> pure pt
    shellFg "nc" ["localhost", show port]

data ServerState = SERVER_STATE
    { storeDir       :: FilePath
    , isShuttingDown :: TVar Bool
    , cogHandles     :: TVar (Map CogName (TVar (Maybe MachineHandle)))
    , termSignal     :: MVar ()
    , lmdb           :: DB.LmdbThread
    , hardware       :: HardwareDb
    }

runServer :: Debug => FilePath -> IO ()
runServer storeDir = do
    debugFan "runServer"

    -- Setup plunder interpreter state.
    writeIORef P.vShowPlun (pure . Sire.ReplExe.showPlun)
    modifyIORef' P.state \s -> s { P.stFast = P.jetMatch }

    termSignal <- newEmptyMVar
    for_ [sigTERM, sigINT] $ \sig -> do
        installHandler sig (Catch (putMVar termSignal ())) Nothing

    isShuttingDown <- newTVarIO False
    cogHandles     <- newTVarIO mempty

    let serverState = do
            _  <- getPidFile storeDir
            db <- DB.openDatastore storeDir
            hw <- standardHardware storeDir
            st <- pure SERVER_STATE
                { storeDir       = storeDir
                , isShuttingDown = isShuttingDown
                , cogHandles     = cogHandles
                , lmdb           = db
                , hardware       = hw
                , termSignal     = termSignal
                }
            _c <- runControlServer st
            pure st

    with serverState shutdownOnSigkill

{-
    To boot a machine, we just write the initial value as a single Init
    to the log, and then exit.
-}
doBoot :: Debug => ServerState -> CogName -> Text -> IO ()
doBoot st machineName bootHash = do
    debugText "spun, now we do"

    val <- DB.loadPinByHash st.lmdb (decodeBtc bootHash) >>= \case
               Nothing -> error $ unpack ("NO HASH: " <> bootHash)
               Just pn -> pure pn.pinItem

    DB.loadMachine st.lmdb machineName >>= \case
        ExistingMachine _ ->
            error ( "Trying to overwrite existing machine "
                 ++ show machineName
                  )

        NewMachine -> do
            lb <- buildInitialLogBatch val

            sig <- newEmptyTMVarIO
            let onWrite = putTMVar sig ()
            atomically $ DB.queueWriteLogBatch st.lmdb machineName lb onWrite
            atomically $ takeTMVar sig

buildInitialLogBatch :: Fan -> IO LogBatch
buildInitialLogBatch initVal = do
    writeTime <- getNanoTime

    let batchNum = BatchNum 0
        lastSnapshot = BatchNum 0
        snapshot = Just initVal
        executed = []

    pure LogBatch{..}

{-
    `vShutdownFlag` serves as a guard which prevents new cogs from
    spinning up (and therefore being added to `vHandles` while we are
    trying to shut everything down.

    `vHandles` is a `MachineHandle` for each running cog.  These handles
    are in the `Nothing` state while starting up.

    This assumes that a cog will never fail to spin up.  Make sure that
    is true!
-}
shutdownOnSigkill :: Debug => ServerState -> IO ()
shutdownOnSigkill st = do
    readMVar st.termSignal

    handles <-
        atomically do
            writeTVar st.isShuttingDown True
            readTVar st.cogHandles

    debugText "Beginning shutdown"

    -- Ask each cog in the machine to halt, and wait for their thread
    -- to exit.
    forConcurrently_ (mapToList handles)
        \(cogNm, vHandle) -> do
            debugText ("Asking cog to stop: " <> cogNm.txt)
            handul <- atomically do
                handul <- readTVar vHandle >>= maybe retry pure
                writeTQueue' (thControlQ handul) MachineEventSnapshotAndShutdown
                pure handul
            debugText ("Waiting for cog to stop: " <> cogNm.txt)
            waitCatch (thAsync handul) >>= \case
                Right () -> debugText ("Cog halted successfully: " <> cogNm.txt)
                Left err -> debugText $ unlines [ "Cog halted with error: "
                                                   <> cogNm.txt
                                                , tshow err ]

    debugText "Finished shutdownOnSigKill"

listCogs :: Debug => FilePath -> Bool -> IO ()
listCogs d showStatus = do
    withDaemon d $ do
        status <- reqCogs
        liftIO $ showStatus & \case
            False ->  debugText (intercalate " " ((.txt) <$> keys status))
            True  ->  for_ (mapToList status) \(k,v) -> do
                          debugText (k.txt <> "\t" <> toLower (tshow v))

spinCog :: Debug => ServerState -> ReplayFrom -> CogName -> IO ()
spinCog st replayFrom nm = do
    debugTextVal "spinCog_cogName" nm.txt

    DB.loadMachine st.lmdb nm >>= \case
        NewMachine -> do
            throwIO (NO_SUCH_COG nm)
        ExistingMachine _bn -> do
            vHandle <- join $ atomically do
                halty <- readTVar st.isShuttingDown
                table <- readTVar st.cogHandles
                case (halty, lookup nm table) of
                    (True, _) ->
                        pure $ throwIO (SPIN_DURING_SHUTDOWN nm)
                    (_, Just{}) ->
                        pure $ throwIO (COG_ALREADY_SPINNING nm)
                    (False, Nothing) -> do
                        h <- newTVar Nothing
                        modifyTVar' st.cogHandles (insertMap nm h)
                        pure (pure h)
            --
            debugText "FOUND COG, STARTING REPLAY"
            h <- replayMachine (routerFun st.hardware) st.lmdb replayFrom nm
            atomically (writeTVar vHandle (Just h))
            debugText "COG SPINNING"

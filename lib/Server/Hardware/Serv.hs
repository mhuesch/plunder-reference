{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# OPTIONS_GHC -Wall            #-}
{-# OPTIONS_GHC -Werror          #-}

-- TODO Cancelation Flows
-- TODO Implement SERV requests.
-- TODO Better testing flow (interation is too slow atm)
-- TODO Test multiple SERV requests.
-- TODO Restrict incoming request to localhost-only.
-- TODO Open any old http port (not 8888) and write out a .ports file.

{-|

    Hardware Device 1 -- Localhost Static HTTP

    There are two requests:

    ```
    SERV :: Tab (Cord, Bar) -> IO ReturnCode
    OPEN :: Cord -> IO Handle
    ```

    Requests are encoded as `(SERV x)=(0 x)` and `(OPEN n)=(1 n)`.

    `SERV` sets the static content to be served:

    -   The tab keys are matched against filepath cords
        (e.g. "/index.html").

    -   The tab values are a pair of a content content-type and a payload,
        the content type is a nat, and the payload is a bar.

        All the content-type values must non-empty cords containing only
        printable ASCII characters.  Invalid requests will get an
        immediate `0` response.

        Paths that correspond to websocket addresses `/ws(/.*)?`
        are ignored.

    -   Valid SERV requests never return, but may be canceled to stop
        serving the content.

    -   If there are multiple SERV requests active at the same time,
        the hardware will refuse to serve anything (returning a 500
        HTTP response).

    `OPEN` listens for a websocket request.

    -   The `key` of `(OPEN key)` must be a cord made up of printable
        ASCII characters.   `OPEN 0` listens at `/ws/`. `OPEN "str"`
        listens at `/ws/str`.

    -   An invalid `OPEN` request will return immediately with `0`.

    -   If multiple OPEN requests exist on the same path, all connections
        at that path will be refused (with a 500 HTTP response).

    -   When a web-socket connection is matched with a request, a new
        hardware device is spawned to interact with the socket.

    Implementation
    ==============

    - There is a single active server thread that is always running.

    - Incoming requests directly mutate the state, no input queue.

    - The state is an association list from key to static site.

    - The server examines this table on every request.  If it is empty,
      we 404, if it has more than one thing, we 500.

-}

module Server.Hardware.Serv where

import PlunderPrelude
import Plun.Eval
import Server.Hardware.Types
import Server.Debug

import Data.Acquire         (Acquire, mkAcquire)
import Server.Convert       (FromNoun(..), fromNoun)
import Server.Types.Logging (CogName(..))
import System.Directory     (removeFile)
import System.IO.Error      (catchIOError)

import qualified Data.Char                      as C
import qualified Network.HTTP.Types             as W
import qualified Network.Socket                 as N
import qualified Network.Wai                    as W
import qualified Network.Wai.Handler.Warp       as W
import qualified Network.Wai.Handler.WebSockets as W
import qualified Network.WebSockets             as WS

--------------------------------------------------------------------------------

data RawRequest = RR
    { cogName  :: !CogName
    , val      :: !Fan
    , callback :: !(TVar (Maybe (Fan -> STM ())))
    }

data PathLeaf = PL
    { contentType :: !ByteString
    , body        :: !File
    }

newtype File = FILE { bytes :: LByteString }

newtype StaticSite = SS
    { files :: Map ByteString PathLeaf }

--------------------------------------------------------------------------------

getTab :: Fan -> Maybe (Map Nat Fan)
getTab (TAB x) = pure x
getTab _       = Nothing

instance FromNoun File where
    fromNoun (PIN p) = fromNoun p.pinItem
    fromNoun (BAR b) = Just (FILE $ fromStrict b)
    fromNoun _       = Nothing

instance FromNoun StaticSite where
    fromNoun (PIN p) = fromNoun p.pinItem
    fromNoun fan = do
        t <- getTab fan
        go mempty (mapToList t)
      where
        okContentType :: ByteString -> Bool
        okContentType ct =
            not (null ct) && all (C.isPrint . C.chr . fromIntegral) ct

        go :: Map ByteString PathLeaf -> [(Nat, Fan)] -> Maybe StaticSite
        go !acc []          = pure (SS acc)
        go !acc ((n,v):nvs) = do
            (cTyNat, body) <- fromNoun @(Nat,File) v
            let path = natBytes n
            let cTy = natBytes cTyNat
            guard (okContentType cTy)
            go (insertMap path (PL cTy body) acc) nvs

--------------------------------------------------------------------------------

data CogState = COG_STATE
    { sock :: N.Socket
    , live :: TVar [(StaticSite, Callback ())]
    , wipe :: Async ()
    , serv :: Async ()
    , port :: Int
    , file :: FilePath
    }

data HWState = HW_STATE
    { mach  :: FilePath
    , wsApp :: CogName -> WS.ServerApp
    , cogs  :: TVar (Map CogName CogState)
    , inpQ  :: TBQueue RawRequest
    , inpT  :: Async Void
    }

wipeThread :: CogState -> IO ()
wipeThread cog =
    forever $ atomically (readTVar cog.live >>= loop [])
  where
    loop !_   []     = retry
    loop !acc (x@(_,k):xs) =
        isCanceled k >>= \case
            True  -> writeTVar cog.live $! (acc <> xs)
            False -> loop (x:acc) xs

-- TODO Tolerate exceptions from `removeFile`.
cancelCog :: CogState -> IO ()
cancelCog cog = do
    cancel cog.wipe
    cancel cog.serv
    catchIOError (removeFile cog.file) (const $ pure ())

startCog :: Debug => HWState -> CogName -> IO CogState
startCog st cogName = do
        -- TODO Only accept input from localhost
        let localhost = N.tupleToHostAddress (0x7f, 0, 0, 1)
        let flags = [N.AI_NUMERICHOST, N.AI_NUMERICSERV]
        let tcp   = 6
        let addr  = (N.SockAddrInet 0 localhost)
        let ainfo = N.AddrInfo flags N.AF_INET N.Stream tcp addr Nothing
        listenSocket <- N.openSocket ainfo
        N.listen listenSocket 5 -- TODO Should this be 5?
        listenPort <- fromIntegral <$> N.socketPort listenSocket
        debugTextVal "HTTP_PORT" (tshow listenPort)

        let baseName  = cogName.txt <> ".http.port"
        let portsFile = st.mach </> unpack baseName
        debugTextVal "HTTP_PORT_FILE" (pack portsFile)
        writeFileUtf8 portsFile (tshow listenPort)

        mdo
            sock <- pure listenSocket
            port <- pure listenPort
            live <- newTVarIO []
            wipe <- async (wipeThread stat)
            serv <- async (servThread (st.wsApp cogName) stat)
            stat <- pure (COG_STATE sock live wipe serv port portsFile)
            pure stat

servThread :: WS.ServerApp -> CogState -> IO ()
servThread wsApp cog = do
    let set = W.defaultSettings & W.setPort (fromIntegral cog.port)
    W.runSettingsSocket set cog.sock
        $ W.websocketsOr WS.defaultConnectionOptions wsApp
        $ \req k -> do
            let pax = req.rawPathInfo
            atomically (readTVar cog.live) >>= \case
                []       -> k notFound
                _:_:_    -> k conflict
                [(ss,_)] -> case lookup pax ss.files of
                                Nothing -> k notFound
                                Just lf -> k (okay lf.contentType lf.body.bytes)
  where
    notFound   = W.responseLBS W.notFound404 [] "not found"
    conflict   = W.responseLBS W.status500 [] "conlict"
    okay ty bd = W.responseLBS W.ok200 [("Content-Type",ty)] bd

getCog :: Debug => HWState -> CogName -> IO CogState
getCog st cogName = do
    cogs <- atomically (readTVar st.cogs)
    case lookup cogName cogs of
        Just cog -> pure cog
        Nothing -> do
            cog <- startCog st cogName
            atomically $ modifyTVar' st.cogs (insertMap cogName cog)
            pure cog

routeRequest :: Debug => HWState -> RawRequest -> IO ()
routeRequest st raw = do
    cog <- getCog st raw.cogName
    let k = CB (\() -> NAT 0) raw.callback
    atomically $
        case fromNoun raw.val of
            Nothing -> callback k () -- TODO
            Just ss -> modifyTVar' cog.live ((ss,k):)

createHardwareServ
    :: Debug
    => FilePath
    -> (CogName -> WS.ServerApp)
    -> Acquire HardwareFun
createHardwareServ dir wsApp = do
    st <- mkAcquire startup shutdown
    pure \m v k -> writeTBQueue st.inpQ (RR m v k)
  where
    shutdown :: HWState -> IO ()
    shutdown st = do
        cancel st.inpT
        cogs <- atomically (readTVar st.cogs)
        for_ cogs cancelCog

    startup :: IO HWState
    startup = do
        inpQ <- newTBQueueIO 100
        cogs <- newTVarIO mempty
        mdo
            let step = (atomically (readTBQueue inpQ) >>= routeRequest stat)
            inpT <- async $ forever step
            stat <- pure (HW_STATE dir wsApp cogs inpQ inpT)
            pure stat

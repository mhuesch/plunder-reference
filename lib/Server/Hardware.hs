module Server.Hardware
    ( buildHardwareDb
    , routerFun
    , standardHardware
    )
where

import Data.Acquire
import PlunderPrelude
import Server.Hardware.Types
import Server.Types.Logging
import Server.Debug

import Plun                      (Fan(NAT), Nat, (%%))
import Server.Hardware.Port      (createHardwarePort)
import Server.Hardware.Rand      (createHardwareRand)
import Server.Hardware.Serv      (createHardwareServ)
import Server.Hardware.Telnet    (createHardwareTelnet)
import Server.Hardware.Time      (createHardwareTime)
import Server.Hardware.WebSocket (createHardwareWebSocket)

import qualified StmContainers.Map as SM

--------------------------------------------------------------------------------

buildHardwareDb :: [(Nat, HardwareFun)] -> IO HardwareDb
buildHardwareDb inputs = do
    table <- SM.newIO
    atomically $ forM_ inputs \(i,hf) -> SM.insert hf i table
    pure (HARDWARE table)

routerFun :: HardwareDb -> RouterFun
routerFun hw hardwareId srcName msg cb = do
    mybDst <- SM.lookup hardwareId hw.table
    case mybDst of
        Just fun -> do
            fun srcName msg cb
        Nothing -> do
            -- In the case of unrecognized hardware, immediately send
            -- back a 0 response.
            readTVar cb >>= \case
                Nothing -> pure ()
                Just k  -> k (NAT 0)

standardHardware :: Debug => FilePath -> Acquire HardwareDb
standardHardware machineDir = do
    hw1_rand           <- createHardwareRand
    (hw4_wsock, wsApp) <- createHardwareWebSocket
    hw2_serv           <- createHardwareServ machineDir wsApp
    hw3_telnet         <- createHardwareTelnet machineDir
    hw5_time           <- createHardwareTime
    hw6_port           <- createHardwarePort
    liftIO . buildHardwareDb $
        [ (1, hw1_rand)
        , (2, hw2_serv)
        , (3, hw3_telnet)
        , (4, hw4_wsock)
        , (5, hw5_time)
        , (6, hw6_port)
        ]

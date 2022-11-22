{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.Hardware.Time where

import PlunderPrelude

import Server.Debug
import Data.Acquire
import Plun.Eval
import Server.Hardware.Types
import Data.UnixTime
import Foreign.C.Types
import Control.Concurrent

--------------------------------------------------------------------------------

data Request
    = WHEN (Callback Nat)
    | WAIT (Callback ()) Nat

createHardwareTime :: Debug => Acquire HardwareFun
createHardwareTime = do
    inQ <- newTBQueueIO 100
    mkAcquire (async $ ferry inQ) cancel
    pure \_machine pln k ->
        case pln of
            NAT 0 -> do
                let cb = CB NAT k
                writeTBQueue inQ (WHEN  cb)
            NAT target -> do
                let cb = CB (const $ NAT 0) k
                writeTBQueue inQ (WAIT cb target)
            _ -> do
                callback (CB id k) (NAT 0)

-- TODO Be careful about overflow
ferry :: Debug => TBQueue Request -> IO Void
ferry inQ = forever do
    atomically (readTBQueue inQ) >>= \case
        WHEN cb -> do
           debugText "I GET TIME"
           t <- getUnixTime
           let CTime c = t.utSeconds
           debugText ("NOW IS " <> tshow c)
           debugText "I GIVE TIME YOU"
           atomically $ callback cb (fromIntegral c)
        WAIT cb target -> do
           debugText "U WANT WAIT?"
           debugText ("THEN IS " <> tshow target)
           debugText "I GET TIME"
           nowUnix <- getUnixTime
           let CTime (fromIntegral -> nowSecs) = nowUnix.utSeconds
           debugText ("NOW IS " <> tshow nowSecs)
           debugText "OK I SLEEP NOW"
           when (nowSecs < target) do
               let diffsecs = fromIntegral (target - nowSecs)
               debugText ("SLEEP FOR SECONDS " <> tshow diffsecs)
               threadDelay (1_000_000 * diffsecs)
           debugText "I FINISH SLEEP.  YOUR TURN"
           atomically $ callback cb ()

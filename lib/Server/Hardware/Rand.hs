{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.Hardware.Rand where

import PlunderPrelude

import Data.Acquire
import Plun.Eval
import Server.Debug
import Server.Hardware.Types

import System.Entropy (getEntropy)

--------------------------------------------------------------------------------

createHardwareRand :: Debug => Acquire HardwareFun
createHardwareRand = do
    inQ <- newTBQueueIO 100
    mkAcquire (start inQ) stop
    pure \m pln k -> do
        let f = maybe (NAT 0) BAR
        writeTBQueue inQ (m, pln, CB f k)
  where
    start inQ = async (randThread inQ)
    stop = cancel

    randThread inQ = forever $ do
      (_, v, cb) <- atomically $ readTBQueue inQ
      debugText "In Rand: "
      res <- case v of
          NAT n -> Just <$> getEntropy (fromIntegral n)
          _     -> pure Nothing
      atomically (callback cb res)

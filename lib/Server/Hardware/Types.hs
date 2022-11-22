{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.Hardware.Types
    ( HardwareDb(..)
    , HardwareFun
    , RouterFun
    , Callback(..)
    , isCanceled
    , callback
    )
where

import Plun                 (Fan)
import PlunderPrelude
import Server.Types.Logging

import qualified StmContainers.Map as SM

--------------------------------------------------------------------------------

-- | All Hardware Devices
data HardwareDb = HARDWARE {
    table :: SM.Map Nat HardwareFun
}

-- Processes can make CALLs to hardware. Right now, none of these requests are
-- cancellable; if you try to cancel one, it just won't give you a response.

-- | The type of the function that individual pieces of hardware must expose.
type HardwareFun =
    ( CogName
   -> Fan
   -> TVar (Maybe (Fan -> STM ()))
   -> STM ()
    )

-- | The hardware router which is exposed to each Machine. This is a superset
-- of HardwareFun and most implementations will pass most arguments verbatim.
type RouterFun =
    ( Nat
   -> CogName
   -> Fan
   -> TVar (Maybe (Fan -> STM ()))
   -> STM ()
    )


-- Callbacks -------------------------------------------------------------------

data Callback a = CB
    { enc :: !(a -> Fan)
    , act :: !(TVar (Maybe (Fan -> STM ())))
    }

isCanceled :: Callback a -> STM Bool
isCanceled cb = maybe True (const False) <$> readTVar cb.act

callback :: Callback a -> a -> STM ()
callback cb val =
    readTVar cb.act >>= \case
        Nothing -> pure ()
        Just fn -> fn (cb.enc val)

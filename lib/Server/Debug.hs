module Server.Debug
    ( debug
    , debugVal
    , debugFan
    , debugFanVal
    , debugText
    , debugTextVal
    , withDebugOutput
    , Debug
    )
where

import PlunderPrelude

import Rex            (colorsOnlyInTerminal)
import Plun.Eval      (Fan)
import Server.Convert (ToNoun(toNoun))
import Loot.ReplExe   (printValue)

--------------------------------------------------------------------------------

type Debug = (?debugOut :: MVar (Maybe Text, Fan))

debug :: (Debug, ToNoun a, MonadIO m) => a -> m ()
debug x = do
    fan <- (evaluate $ force $ toNoun x)
    putMVar ?debugOut (Nothing, fan)

debugText :: (Debug, MonadIO m) => Text -> m ()
debugText = debug

debugTextVal :: (Debug, MonadIO m) => Text -> Text -> m ()
debugTextVal = debugVal

debugVal :: (Debug, ToNoun a, MonadIO m) => Text -> a -> m ()
debugVal nam x = do
    fan <- (evaluate $ force $ toNoun x)
    putMVar ?debugOut (Just nam, fan)

debugFan :: (Debug, MonadIO m) => Fan -> m ()
debugFan = debug

debugFanVal :: (Debug, MonadIO m) => Text -> Fan -> m ()
debugFanVal = debugVal

withDebugOutput :: ∀a. (Debug => IO a) -> IO a
withDebugOutput act =
    colorsOnlyInTerminal do
        out <- newEmptyMVar

        tid <- async $ forever do
                   (nam, fan) <- takeMVar out
                   printValue stdout True (utf8Nat <$> nam) fan

        flip finally (cancel tid) $
            let ?debugOut = out
            in (act :: IO a)

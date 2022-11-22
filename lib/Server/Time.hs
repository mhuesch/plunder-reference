module Server.Time where

import PlunderPrelude
import Numeric.Natural

import Data.Time.Clock       (NominalDiffTime)
import Data.Time.Clock       (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Time.Clock       (secondsToNominalDiffTime)

nanosSinceEpoch :: POSIXTime -> Natural
nanosSinceEpoch = floor . (1e9 *) . nominalDiffTimeToSeconds

epochNanosToPOSIX :: Natural -> POSIXTime
epochNanosToPOSIX = secondsToNominalDiffTime . (/ 1e9) . fromIntegral

getNanoTime :: MonadIO m => m Natural
getNanoTime = liftIO (nanosSinceEpoch <$> getPOSIXTime)

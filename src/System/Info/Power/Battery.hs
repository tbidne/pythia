-- | This modules exports everything needed for retrieving battery information.
module System.Info.Power.Battery
  ( module X,
    queryBatteryState,
  )
where

import System.Info.Data.Command (Command (..))
import System.Info.Data.Error (Error)
import System.Info.Power.Battery.Types as X
import System.Info.Power.Battery.UPower qualified as UPower
import System.Info.Utils qualified as U

-- | This is the primary function that attempts to use the given
-- program to retrieve battery information.
queryBatteryState :: BatteryProgram -> IO (Either Error BatteryState)
queryBatteryState UPower = U.runShellAndParse UPower.parseBattery UPower.command
queryBatteryState (Custom c) = U.runShellAndParse UPower.parseBattery (MkCommand c)

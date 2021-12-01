-- | This modules exports everything needed for retrieving battery
-- level and charge status.
module System.Info.Services.Battery.State
  ( -- * Types
    BatteryStateApp (..),
    BatteryState (..),
    BatteryLevel,
    ChargeStatus (..),

    -- * Query
    queryBatteryState,
  )
where

import Data.Text (Text)
import Optics.Core ((%), (.~))
import System.Info.Data (Command (..))
import System.Info.Services.Battery.State.UPower qualified as UPower
import System.Info.Services.Battery.Types
  ( BatteryLevel,
    BatteryState (..),
    ChargeStatus (..),
  )
import System.Info.ShellApp (QueryResult, ShellApp (..))
import System.Info.ShellApp qualified as ShellApp

-- | Determines how we should query the system for battery state information.
-- The custom option assumes the same output format as UPower, i.e., the
-- output contains lines like:
--
-- @
-- percentage: 20%
-- state: \<discharging|charging|fully-charged\>
-- @
--
-- @since 0.1.0.0
data BatteryStateApp
  = -- | Uses the UPower utility.
    --
    -- @since 0.1.0.0
    UPower
  | -- | Runs a custom script.
    --
    -- @since 0.1.0.0
    Custom Text
  deriving
    ( -- @since 0.1.0.0
      Eq,
      -- @since 0.1.0.0
      Show
    )

-- | This is the primary function that attempts to use the given
-- program to retrieve battery information.
--
-- >>> queryBatteryState UPower
-- Right (MkBatteryState {level = MkUnsafeBoundedN {unBoundedN = 24}, status = Charging})
--
-- @since 0.1.0.0
queryBatteryState :: BatteryStateApp -> IO (QueryResult BatteryState)
queryBatteryState UPower = ShellApp.runShellApp UPower.batteryStateShellApp
queryBatteryState (Custom c) = ShellApp.runShellApp $ customShellApp c

-- Reuse UPower's parser
customShellApp :: Text -> ShellApp BatteryState
customShellApp cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    UPower.batteryStateShellApp

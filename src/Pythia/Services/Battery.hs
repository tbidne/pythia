-- | This module exports battery related services.
--
-- @since 0.1.0.0
module Pythia.Services.Battery
  ( -- * State
    queryBatteryState,
    BatteryStateApp (..),
    BatteryState (..),
    BatteryLevel,

    -- * Misc Types
    QueryResult,
    QueryError (..),
  )
where

import Pythia.Data (Command (..), QueryError (..), QueryResult)
import Pythia.Prelude
import Pythia.Services.Battery.Types (BatteryLevel, BatteryState (..))
import Pythia.Services.Battery.UPower qualified as UPower
import Pythia.ShellApp (ShellApp (..))
import Pythia.ShellApp qualified as ShellApp

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
    BatteryStateUPower
  | -- | Runs a custom script.
    --
    -- @since 0.1.0.0
    BatteryStateCustom Text
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
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
queryBatteryState BatteryStateUPower = ShellApp.runShellApp UPower.batteryStateShellApp
queryBatteryState (BatteryStateCustom c) = ShellApp.runShellApp $ customShellApp c

-- Reuse UPower's parser
customShellApp :: Text -> ShellApp BatteryState
customShellApp cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    UPower.batteryStateShellApp

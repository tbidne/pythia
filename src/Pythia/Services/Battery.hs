-- | This module exports battery related services.
--
-- @since 0.1.0.0
module Pythia.Services.Battery
  ( -- * State
    queryBattery,
    BatteryApp (..),
    Battery (..),
    BatteryLevel,

    -- * Misc Types
    QueryResult,
    QueryError (..),
  )
where

import Pythia.Data (Command (..), QueryError (..), QueryResult)
import Pythia.Prelude
import Pythia.Services.Battery.Types (Battery (..), BatteryLevel)
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
data BatteryApp
  = -- | Uses the UPower utility.
    --
    -- @since 0.1.0.0
    BatteryUPower
  | -- | Runs a custom script.
    --
    -- @since 0.1.0.0
    BatteryCustom Text
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | This is the primary function that attempts to use the given
-- program to retrieve battery information.
--
-- >>> queryBattery UPower
-- Right (MkBattery {level = MkUnsafeBoundedN {unBoundedN = 24}, status = Charging})
--
-- @since 0.1.0.0
queryBattery :: BatteryApp -> IO (QueryResult Battery)
queryBattery BatteryUPower = ShellApp.runShellApp UPower.batteryShellApp
queryBattery (BatteryCustom c) = ShellApp.runShellApp $ customShellApp c

-- Reuse UPower's parser
customShellApp :: Text -> ShellApp Battery
customShellApp cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    UPower.batteryShellApp

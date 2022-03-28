-- | This module exports battery related services.
--
-- @since 0.1.0.0
module Pythia.Services.Battery
  ( -- * Queries
    queryBattery,
    queryBatteryApp,

    -- * Types
    BatteryApp (..),
    module Pythia.Services.Battery.Types,
  )
where

import Pythia.Data (Command (..), QueryResult)
import Pythia.Prelude
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.SysFs qualified as SysFs
import Pythia.Services.Battery.Types (Battery (..), BatteryLevel, BatteryStatus (..))
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
  = -- | Uses the ACPI utility.
    --
    -- @since 0.1.0.0
    BatteryAcpi
  | -- | Uses the sysfs interface i.e. /sys.
    --
    -- @since 0.1.0.0
    BatterySysFs
  | -- | Uses the UPower utility.
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

-- | Attempts to query for battery information by detecting supported
-- apps. Tries, in the following order: ['BatterySysFs', 'BatteryAcpi',
-- 'BatteryUPower']
--
-- @since 0.1.0.0
queryBattery :: IO (QueryResult Battery)
queryBattery = ShellApp.tryApps toShellApp allApps
  where
    allApps =
      [ (BatterySysFs, SysFs.supported),
        (BatteryAcpi, Acpi.supported),
        (BatteryUPower, UPower.supported)
      ]

-- | This is the primary function that attempts to use the given
-- program to retrieve battery information.
--
-- >>> queryBattery UPower
-- Right (MkBattery {level = MkUnsafeBoundedN {unBoundedN = 24}, status = Charging})
--
-- @since 0.1.0.0
queryBatteryApp :: BatteryApp -> IO (QueryResult Battery)
queryBatteryApp = ShellApp.runShellApp . toShellApp

toShellApp :: BatteryApp -> ShellApp Battery
toShellApp BatteryAcpi = Acpi.batteryShellApp
toShellApp BatterySysFs = SysFs.batteryShellApp
toShellApp BatteryUPower = UPower.batteryShellApp
toShellApp (BatteryCustom c) = customShellApp c

-- Reuse UPower's parser
customShellApp :: Text -> ShellApp Battery
customShellApp cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    UPower.batteryShellApp

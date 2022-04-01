-- | This module exports battery related services.
--
-- @since 0.1.0.0
module Pythia.Services.Battery
  ( -- * Queries
    queryBattery,
    queryBatteryConfig,

    -- * Types
    BatteryApp (..),
    module Pythia.Services.Battery.Types,
  )
where

import Pythia.Data (QueryResult, RunApp (..))
import Pythia.Prelude
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.SysFs qualified as SysFs
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryApp (..),
    BatteryConfig (..),
    BatteryLevel,
    BatteryStatus (..),
  )
import Pythia.Services.Battery.UPower qualified as UPower
import Pythia.ShellApp (ShellApp (..))
import Pythia.ShellApp qualified as ShellApp

-- | Attempts to query for battery information by detecting supported
-- apps. Tries, in the following order: ['BatterySysFs', 'BatteryAcpi',
-- 'BatteryUPower']
--
-- @since 0.1.0.0
queryBattery :: IO (QueryResult Battery)
queryBattery = queryBatteryConfig mempty

-- | This is the primary function that attempts to use the given
-- program to retrieve battery information.
--
-- >>> queryBattery UPower
-- Right (MkBattery {level = MkUnsafeBoundedN {unBoundedN = 24}, status = Charging})
--
-- @since 0.1.0.0
-- queryBatteryApp :: BatteryApp -> IO (QueryResult Battery)
-- queryBatteryApp = ShellApp.runShellApp . toShellApp
queryBatteryConfig :: BatteryConfig -> IO (QueryResult Battery)
queryBatteryConfig config =
  case config ^. #batteryApp of
    Many -> ShellApp.tryIOs allApps
    Single app -> ShellApp.runShellApp $ toShellApp app
  where
    allApps =
      [ (singleRun BatterySysFs, SysFs.supported),
        (singleRun BatteryAcpi, Acpi.supported),
        (singleRun BatteryUPower, UPower.supported)
      ]
    singleRun :: BatteryApp -> IO (QueryResult Battery)
    singleRun = ShellApp.runShellApp . toShellApp

toShellApp :: BatteryApp -> ShellApp Battery
toShellApp BatteryAcpi = Acpi.batteryShellApp
toShellApp BatterySysFs = SysFs.batteryShellApp
toShellApp BatteryUPower = UPower.batteryShellApp

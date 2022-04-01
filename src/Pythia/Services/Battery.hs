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

import Pythia.Data (RunApp (..))
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
import Pythia.ShellApp (AppAction (..))
import Pythia.ShellApp qualified as ShellApp

-- | Attempts to query for battery information by detecting supported
-- apps. Tries, in the following order: ['BatterySysFs', 'BatteryAcpi',
-- 'BatteryUPower']
--
-- @since 0.1.0.0
queryBattery :: IO Battery
queryBattery = queryBatteryConfig mempty

-- | This is the primary function that attempts to use the given
-- program to retrieve battery information.
--
-- >>> queryBattery UPower
-- Right (MkBattery {level = MkUnsafeBoundedN {unBoundedN = 24}, status = Charging})
--
-- @since 0.1.0.0
queryBatteryConfig :: BatteryConfig -> IO Battery
queryBatteryConfig config =
  case config ^. #batteryApp of
    Many -> ShellApp.tryAppActions allApps
    Single app -> toShellApp app
  where
    allApps =
      [ MkAppAction (toShellApp BatterySysFs) SysFs.supported (show BatterySysFs),
        MkAppAction (toShellApp BatteryAcpi) Acpi.supported (show BatteryAcpi),
        MkAppAction (toShellApp BatteryUPower) UPower.supported (show BatteryUPower)
      ]

toShellApp :: BatteryApp -> IO Battery
toShellApp BatteryAcpi = Acpi.batteryShellApp
toShellApp BatterySysFs = SysFs.batteryQuery
toShellApp BatteryUPower = UPower.batteryShellApp

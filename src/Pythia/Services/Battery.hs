-- | This module exports battery related services.
--
-- @since 0.1
module Pythia.Services.Battery
  ( -- * Queries
    queryBattery,
    queryBatteryConfig,

    -- * Types
    Battery (..),
    BatteryPercentage (..),
    BatteryStatus (..),

    -- ** Configuration
    BatteryConfig (..),
    BatteryApp (..),
    RunApp (..),

    -- ** Errors
    AcpiException (..),
    UPowerException (..),
    SysFsException (..),
  )
where

import Pythia.Data.RunApp (RunApp (..))
import Pythia.Prelude
import Pythia.Services.Battery.Acpi (AcpiException (..))
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.SysFs (SysFsException (..))
import Pythia.Services.Battery.SysFs qualified as SysFs
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryApp (..),
    BatteryConfig (..),
    BatteryPercentage (..),
    BatteryStatus (..),
  )
import Pythia.Services.Battery.UPower (UPowerException (..))
import Pythia.Services.Battery.UPower qualified as UPower
import Pythia.ShellApp (AppAction (..))
import Pythia.ShellApp qualified as ShellApp

-- | Queries for battery information with default configuration.
--
-- Throws 'Pythia.Control.Exception.PythiaException' if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryBattery :: (MonadCatch m, MonadIO m) => m Battery
queryBattery = queryBatteryConfig mempty

-- | Queries the battery based on the configuration. If 'batteryApp' is
-- 'Many' then we try supported apps in the following order:
--
-- @
-- ['BatterySysFs', 'BatteryAcpi', 'BatteryUPower']
-- @
--
-- Throws 'Pythia.Control.Exception.PythiaException' if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryBatteryConfig :: (MonadCatch m, MonadIO m) => BatteryConfig -> m Battery
queryBatteryConfig config =
  case config ^. #batteryApp of
    Many -> ShellApp.tryAppActions allApps
    Single app -> toShellApp app
  where
    allApps =
      [ MkAppAction (toShellApp BatterySysFs) SysFs.supported (showt BatterySysFs),
        MkAppAction (toShellApp BatteryAcpi) Acpi.supported (showt BatteryAcpi),
        MkAppAction (toShellApp BatteryUPower) UPower.supported (showt BatteryUPower)
      ]

toShellApp :: (MonadCatch m, MonadIO m) => BatteryApp -> m Battery
toShellApp BatteryAcpi = Acpi.batteryShellApp
toShellApp BatterySysFs = SysFs.batteryQuery
toShellApp BatteryUPower = UPower.batteryShellApp

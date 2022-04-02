-- | This module exports battery related services.
--
-- @since 0.1.0.0
module Pythia.Services.Battery
  ( -- * Queries
    queryBattery,
    queryBatteryConfig,

    -- * Types
    Battery (..),
    BatteryPercentage,
    BatteryStatus (..),

    -- ** Configuration
    BatteryConfig (..),
    BatteryApp (..),

    -- ** Errors
    uncheckBattery,
    AcpiError (..),
    UPowerError (..),
    SysFsError (..),
  )
where

import Pythia.Data (RunApp (..))
import Pythia.Prelude
import Pythia.Services.Battery.Acpi (AcpiError (..))
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.SysFs (SysFsError (..))
import Pythia.Services.Battery.SysFs qualified as SysFs
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryApp (..),
    BatteryConfig (..),
    BatteryPercentage,
    BatteryStatus (..),
  )
import Pythia.Services.Battery.UPower (UPowerError (..))
import Pythia.Services.Battery.UPower qualified as UPower
import Pythia.ShellApp (AppAction (..), CmdError (..), Exceptions (..))
import Pythia.ShellApp qualified as ShellApp

-- | Attempts to query for battery information by detecting supported
-- apps. Tries, in the following order: ['BatterySysFs', 'BatteryAcpi',
-- 'BatteryUPower']
--
-- @since 0.1.0.0
queryBattery ::
  ( Throws AcpiError,
    Throws CmdError,
    Throws Exceptions,
    Throws SysFsError,
    Throws UPowerError
  ) =>
  IO Battery
queryBattery = queryBatteryConfig mempty

-- | Queries the battery based on the configuration.
--
-- @since 0.1.0.0
queryBatteryConfig ::
  ( Throws AcpiError,
    Throws CmdError,
    Throws Exceptions,
    Throws SysFsError,
    Throws UPowerError
  ) =>
  BatteryConfig ->
  IO Battery
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

toShellApp ::
  ( Throws AcpiError,
    Throws CmdError,
    Throws SysFsError,
    Throws UPowerError
  ) =>
  BatteryApp ->
  IO Battery
toShellApp BatteryAcpi = Acpi.batteryShellApp
toShellApp BatterySysFs = SysFs.batteryQuery
toShellApp BatteryUPower = UPower.batteryShellApp

-- | Unchecks all exceptions returns by battery queries.
--
-- @since 0.1.0.0
uncheckBattery ::
  ( ( Throws AcpiError,
      Throws CmdError,
      Throws Exceptions,
      Throws SysFsError,
      Throws UPowerError
    ) =>
    IO a
  ) ->
  IO a
uncheckBattery = uncheck5 @AcpiError @CmdError @Exceptions @SysFsError @UPowerError

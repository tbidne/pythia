-- | This module exports battery related services.
--
-- @since 0.1.0.0
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
    BatteryException (..),
    uncheckBattery,
    rethrowBattery,

    -- *** Sub-errors
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
    BatteryException (..),
    BatteryPercentage (..),
    BatteryStatus (..),
  )
import Pythia.Services.Battery.UPower (UPowerException (..))
import Pythia.Services.Battery.UPower qualified as UPower
import Pythia.ShellApp (AppAction (..), MultiExceptions (..))
import Pythia.ShellApp qualified as ShellApp

-- | Attempts to query for battery information by detecting supported
-- apps. Tries, in the following order: ['BatterySysFs', 'BatteryAcpi',
-- 'BatteryUPower']
--
-- @since 0.1.0.0
queryBattery ::
  ( MonadCatch m,
    MonadIO m,
    Throws BatteryException
  ) =>
  m Battery
queryBattery = queryBatteryConfig mempty

-- | Queries the battery based on the configuration.
--
-- @since 0.1.0.0
queryBatteryConfig ::
  ( MonadCatch m,
    MonadIO m,
    Throws BatteryException
  ) =>
  BatteryConfig ->
  m Battery
queryBatteryConfig config =
  case config ^. #batteryApp of
    Many -> rethrowBattery @MultiExceptions $ ShellApp.tryAppActions allApps
    Single app -> toShellApp app
  where
    allApps =
      [ MkAppAction (toShellApp BatterySysFs) SysFs.supported (show BatterySysFs),
        MkAppAction (toShellApp BatteryAcpi) Acpi.supported (show BatteryAcpi),
        MkAppAction (toShellApp BatteryUPower) UPower.supported (show BatteryUPower)
      ]

toShellApp ::
  ( MonadCatch m,
    MonadIO m,
    Throws BatteryException
  ) =>
  BatteryApp ->
  m Battery
toShellApp BatteryAcpi = rethrowBattery @AcpiException Acpi.batteryShellApp
toShellApp BatterySysFs = rethrowBattery @SysFsException SysFs.batteryQuery
toShellApp BatteryUPower = rethrowBattery @UPowerException UPower.batteryShellApp

-- | 'uncheck' specialized to 'BatteryException'.
--
-- @since 0.1.0.0
uncheckBattery :: ((Throws BatteryException) => m a) -> m a
uncheckBattery = uncheck (Proxy @BatteryException)

-- | Rethrows a checked exception as a 'BatteryException'.
--
-- @since 0.1.0.0
rethrowBattery :: forall e m a. (Exception e, MonadCatch m, Throws BatteryException) => (Throws e => m a) -> m a
rethrowBattery = handle (\(ex :: e) -> throw $ MkBatteryErr ex)

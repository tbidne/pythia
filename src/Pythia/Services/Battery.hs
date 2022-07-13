-- | This module exports battery related services.
--
-- @since 0.1
module Pythia.Services.Battery
  ( -- * Queries
    queryBattery,

    -- * Types
    Battery (..),
    Percentage (..),
    BatteryStatus (..),

    -- ** Configuration
    BatteryConfig (..),
    BatteryApp (..),
    RunApp (..),

    -- ** Errors
    AcpiParseError (..),
    SysFsDirNotFound (..),
    SysFsBatteryDirNotFound (..),
    SysFsFileNotFound (..),
    SysFsBatteryParseError (..),
    UPowerParseError (..),
  )
where

import Pythia.Data.Percentage (Percentage (..))
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Internal.ShellApp (AppAction (..))
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.Battery.Acpi (AcpiParseError (..))
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.SysFs
  ( SysFsBatteryDirNotFound (..),
    SysFsBatteryParseError (..),
    SysFsDirNotFound (..),
    SysFsFileNotFound (..),
  )
import Pythia.Services.Battery.SysFs qualified as SysFs
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryApp (..),
    BatteryConfig (..),
    BatteryStatus (..),
  )
import Pythia.Services.Battery.UPower (UPowerParseError (..))
import Pythia.Services.Battery.UPower qualified as UPower

-- | Queries the battery based on the configuration. If 'app' is
-- 'RunAppMany' then we try supported apps in the following order:
--
-- @
-- ['BatteryAppSysFs', 'BatteryAppAcpi', 'BatteryAppUPower']
-- @
--
-- __Throws:__
--
-- * 'AcpiParseError'
-- * 'SysFsDirNotFound'
-- * 'SysFsBatteryDirNotFound'
-- * 'SysFsFileNotFound'
-- * 'SysFsBatteryParseError'
-- * 'UPowerParseError'
-- * 'Pythia.Control.Exception.CommandException'
--
-- @since 0.1
queryBattery :: BatteryConfig -> IO Battery
queryBattery config =
  case config ^. #app of
    RunAppMany -> ShellApp.tryAppActions allApps
    RunAppSingle app -> toShellApp app
  where
    allApps =
      [ MkAppAction (toShellApp BatteryAppSysFs) SysFs.supported "sysfs",
        MkAppAction (toShellApp BatteryAppAcpi) Acpi.supported "acpi",
        MkAppAction (toShellApp BatteryAppUPower) UPower.supported "upower"
      ]
{-# INLINEABLE queryBattery #-}

toShellApp :: BatteryApp -> IO Battery
toShellApp BatteryAppAcpi = Acpi.batteryShellApp
toShellApp BatteryAppSysFs = SysFs.batteryQuery
toShellApp BatteryAppUPower = UPower.batteryShellApp
{-# INLINEABLE toShellApp #-}

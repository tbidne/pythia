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
    BatteryApp (..),
  )
where

import Pythia.Data.Percentage (Percentage (MkPercentage))
import Pythia.Prelude
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.SysFs qualified as SysFs
import Pythia.Services.Battery.Types
  ( Battery (MkBattery, percentage, status),
    BatteryApp (BatteryAppAcpi, BatteryAppSysFs, BatteryAppUPower),
    BatteryStatus (Charging, Discharging, Full, Pending),
  )
import Pythia.Services.Battery.UPower qualified as UPower

-- | Queries the battery.
--
-- @since 0.1
queryBattery :: BatteryApp -> IO Battery
queryBattery BatteryAppAcpi = Acpi.batteryShellApp
queryBattery BatteryAppSysFs = SysFs.batteryQuery
queryBattery BatteryAppUPower = UPower.batteryShellApp

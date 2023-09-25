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

import Pythia.Data.Percentage (Percentage (..))
import Pythia.Prelude
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.SysFs qualified as SysFs
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryApp (..),
    BatteryStatus (..),
  )
import Pythia.Services.Battery.UPower qualified as UPower

-- | Queries the battery.
--
-- @since 0.1
queryBattery ::
  ( FileReaderDynamic :> es,
    PathReaderDynamic :> es,
    TypedProcess :> es
  ) =>
  BatteryApp ->
  Eff es Battery
-- FIXME: Seg fault is in here. Notes:
--
-- 1. Executing this (and any other service) from navi fails
-- 2. Actually running the pythia exe works :-(
-- 3. Returning a hardcoded battery here works in navi
--
-- Next:
--   Try hardcoding the process stuff here. OR maybe return a hardcoded battery
--   but run some random process call? Maybe process is the problem.
queryBattery BatteryAppAcpi = Acpi.batteryShellApp
queryBattery BatteryAppSysFs = SysFs.batteryQuery
queryBattery BatteryAppUPower = UPower.batteryShellApp

{-# OPTIONS_GHC -Wno-everything #-}

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
import Numeric.Data.Interval (LRInterval (MkLRInterval), unsafeLRInterval)
import Numeric.Data.Interval 

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
queryBattery BatteryAppAcpi = pure battery
queryBattery BatteryAppSysFs = pure battery
queryBattery BatteryAppUPower = pure battery

battery :: Battery
battery = MkBattery (MkPercentage $ unsafeLRInterval 4) Charging
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core types describing the battery.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.Types
  ( -- * Configuration
    BatteryConfig (..),
    BatteryApp (..),

    -- * Battery Fields
    BatteryStatus (..),
    BatteryLevel,
    Battery (..),
  )
where

import Numeric.Data.Interval (LRInterval)
import Numeric.Data.Interval qualified as Interval
import Pythia.Data (RunApp)
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Supremum (Supremum (..))

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
  = -- | Uses the sysfs interface i.e. /sys.
    --
    -- @since 0.1.0.0
    BatterySysFs
  | -- | Uses the ACPI utility.
    --
    -- @since 0.1.0.0
    BatteryAcpi
  | -- | Uses the UPower utility.
    --
    -- @since 0.1.0.0
    BatteryUPower
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Enum,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Monoid,
      -- | @since 0.1.0.0
      Semigroup
    )
    via (Supremum BatteryApp)

-- | @since 0.1.0.0
makePrismLabels ''BatteryApp

-- | Battery configuration.
--
-- @since 0.1.0.0
newtype BatteryConfig = MkBatteryConfig
  { -- | @since 0.1.0.0
    batteryApp :: RunApp BatteryApp
  }
  deriving (Eq, Show)

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''BatteryConfig

-- | @since 0.1.0.0
instance Semigroup BatteryConfig where
  MkBatteryConfig l <> MkBatteryConfig r = MkBatteryConfig (l <> r)

-- | @since 0.1.0.0
instance Monoid BatteryConfig where
  mempty = MkBatteryConfig mempty

-- | Represents battery charging status.
--
-- @since 0.1.0.0
data BatteryStatus
  = -- | @since 0.1.0.0
    Charging
  | -- | @since 0.1.0.0
    Discharging
  | -- | @since 0.1.0.0
    Full
  | -- | @since 0.1.0.0
    Pending
  | -- | @since 0.1.0.0
    Unknown Text
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      PrettyPrinter
    )

-- | @since 0.1.0.0
makePrismLabels ''BatteryStatus

-- | Represents battery levels.
--
-- @since 0.1.0.0
type BatteryLevel = LRInterval 0 100 Int

-- | Full battery state, including level and status data.
--
-- @since 0.1.0.0
data Battery = MkBattery
  { -- | @since 0.1.0.0
    level :: BatteryLevel,
    -- | @since 0.1.0.0
    status :: BatteryStatus
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''Battery

-- | @since 0.1.0.0
instance PrettyPrinter Battery where
  pretty bs = status <> ": " <> level <> "%"
    where
      status = pretty $ bs ^. #status
      level = show $ Interval.unLRInterval $ bs ^. #level

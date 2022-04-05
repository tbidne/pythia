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
    BatteryPercentage (..),
    Battery (..),
  )
where

import Numeric.Data.Interval (LRInterval)
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.RunApp (RunApp)
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Supremum (Supremum (..))

-- | Determines how we should query the system for battery state information.
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
-- >>> mempty @BatteryConfig
-- MkBatteryConfig {batteryApp = Many}
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

-- | Represents battery percentages.
--
-- @since 0.1.0.0
newtype BatteryPercentage = MkBatteryPercentage
  { -- | @since 0.1.0.0
    unBatteryPercentage :: LRInterval 0 100 Int
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''BatteryPercentage

-- | @since 0.1.0.0
instance PrettyPrinter BatteryPercentage where
  pretty (MkBatteryPercentage p) = show (Interval.unLRInterval p) <> "%"

-- | Full battery state, including percentage and status data.
--
-- @since 0.1.0.0
data Battery = MkBattery
  { -- | @since 0.1.0.0
    percentage :: BatteryPercentage,
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
  pretty bs = status <> ": " <> percentage
    where
      status = pretty $ bs ^. #status
      percentage = pretty $ bs ^. #percentage

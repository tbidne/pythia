{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core types describing the battery.
--
-- @since 0.1
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
import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Utils (Pretty (..), (<+>))

-- | Determines how we should query the system for battery state information.
--
-- @since 0.1
data BatteryApp
  = -- | Uses the sysfs interface i.e. /sys.
    --
    -- @since 0.1
    BatterySysFs
  | -- | Uses the ACPI utility.
    --
    -- @since 0.1
    BatteryAcpi
  | -- | Uses the UPower utility.
    --
    -- @since 0.1
    BatteryUPower
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Monoid,
      -- | @since 0.1
      Semigroup
    )
    via (Supremum BatteryApp)
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
makePrismLabels ''BatteryApp

-- | Battery configuration.
--
-- >>> mempty @BatteryConfig
-- MkBatteryConfig {batteryApp = Many}
--
-- @since 0.1
newtype BatteryConfig = MkBatteryConfig
  { -- | @since 0.1
    batteryApp :: RunApp BatteryApp
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''BatteryConfig

-- | @since 0.1
instance Semigroup BatteryConfig where
  MkBatteryConfig l <> MkBatteryConfig r = MkBatteryConfig (l <> r)

-- | @since 0.1
instance Monoid BatteryConfig where
  mempty = MkBatteryConfig mempty

-- | Represents battery charging status.
--
-- @since 0.1
data BatteryStatus
  = -- | @since 0.1
    Charging
  | -- | @since 0.1
    Discharging
  | -- | @since 0.1
    Full
  | -- | @since 0.1
    Pending
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrismLabels ''BatteryStatus

-- | @since 0.1
instance Pretty BatteryStatus where
  pretty = pretty . show

-- | Represents battery percentages.
--
-- @since 0.1
newtype BatteryPercentage = MkBatteryPercentage
  { -- | @since 0.1
    unBatteryPercentage :: LRInterval 0 100 Word8
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''BatteryPercentage

-- | @since 0.1
instance Pretty BatteryPercentage where
  pretty (MkBatteryPercentage p) = pretty p <> pretty @Text "%"

-- | Full battery state, including percentage and status data.
--
-- @since 0.1
data Battery = MkBattery
  { -- | @since 0.1
    percentage :: BatteryPercentage,
    -- | @since 0.1
    status :: BatteryStatus
  }
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Battery

-- | @since 0.1
instance Pretty Battery where
  pretty bs =
    status
      <> pretty @Text ":"
      <+> percentage
    where
      status = pretty $ bs ^. #status
      percentage = pretty $ bs ^. #percentage

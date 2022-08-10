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
    Battery (..),

    -- * Optics
    _MkBatteryConfig,
    _BatteryAppAcpi,
    _BatteryAppSysFs,
    _BatteryAppUPower,
    _Charging,
    _Discharging,
    _Full,
    _Pending,
  )
where

import Pythia.Data.Percentage (Percentage)
import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Utils (Pretty (..), (<+>))

-- | Determines how we should query the system for battery state information.
--
-- @since 0.1
type BatteryApp :: Type
data BatteryApp
  = -- | Uses the sysfs interface i.e. /sys.
    --
    -- @since 0.1
    BatteryAppSysFs
  | -- | Uses the ACPI utility.
    --
    -- @since 0.1
    BatteryAppAcpi
  | -- | Uses the UPower utility.
    --
    -- @since 0.1
    BatteryAppUPower
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
makePrisms ''BatteryApp

-- | Battery configuration.
--
-- >>> mempty @BatteryConfig
-- MkBatteryConfig Many
--
-- @since 0.1
type BatteryConfig :: Type
newtype BatteryConfig = MkBatteryConfig (RunApp BatteryApp)
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
makePrisms ''BatteryConfig

-- | @since 0.1
instance Semigroup BatteryConfig where
  MkBatteryConfig l <> MkBatteryConfig r = MkBatteryConfig (l <> r)
  {-# INLINEABLE (<>) #-}

-- | @since 0.1
instance Monoid BatteryConfig where
  mempty = MkBatteryConfig mempty
  {-# INLINEABLE mempty #-}

-- | Represents battery charging status.
--
-- @since 0.1
type BatteryStatus :: Type
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
makePrisms ''BatteryStatus

-- | @since 0.1
instance Pretty BatteryStatus where
  pretty Charging = "Charging"
  pretty Discharging = "Discharging"
  pretty Full = "Full"
  pretty Pending = "Pending"
  {-# INLINEABLE pretty #-}

-- | Full battery state, including percentage and status data.
--
-- @since 0.1
type Battery :: Type
data Battery = MkBattery
  { -- | @since 0.1
    percentage :: Percentage,
    -- | @since 0.1
    status :: BatteryStatus
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
  {-# INLINEABLE pretty #-}

{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core types describing the battery.
--
-- @since 0.1
module Pythia.Services.Battery.Types
  ( -- * Configuration
    BatteryApp (..),

    -- * Battery Fields
    BatteryStatus (..),
    Battery (..),

    -- * Optics
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
import Pythia.Prelude

-- $setup
-- >>> import Pythia.Prelude

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
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
_BatteryAppSysFs :: Prism' BatteryApp ()
_BatteryAppSysFs =
  prism
    (const BatteryAppSysFs)
    ( \x -> case x of
        BatteryAppSysFs -> Right ()
        _ -> Left x
    )
{-# INLINE _BatteryAppSysFs #-}

-- | @since 0.1
_BatteryAppAcpi :: Prism' BatteryApp ()
_BatteryAppAcpi =
  prism
    (const BatteryAppAcpi)
    ( \x -> case x of
        BatteryAppAcpi -> Right ()
        _ -> Left x
    )
{-# INLINE _BatteryAppAcpi #-}

-- | @since 0.1
_BatteryAppUPower :: Prism' BatteryApp ()
_BatteryAppUPower =
  prism
    (const BatteryAppUPower)
    ( \x -> case x of
        BatteryAppUPower -> Right ()
        _ -> Left x
    )
{-# INLINE _BatteryAppUPower #-}

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
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_Charging :: Prism' BatteryStatus ()
_Charging =
  prism
    (const Charging)
    ( \x -> case x of
        Charging -> Right ()
        _ -> Left x
    )
{-# INLINE _Charging #-}

-- | @since 0.1
_Discharging :: Prism' BatteryStatus ()
_Discharging =
  prism
    (const Discharging)
    ( \x -> case x of
        Discharging -> Right ()
        _ -> Left x
    )
{-# INLINE _Discharging #-}

-- | @since 0.1
_Full :: Prism' BatteryStatus ()
_Full =
  prism
    (const Full)
    ( \x -> case x of
        Full -> Right ()
        _ -> Left x
    )
{-# INLINE _Full #-}

-- | @since 0.1
_Pending :: Prism' BatteryStatus ()
_Pending =
  prism
    (const Pending)
    ( \x -> case x of
        Pending -> Right ()
        _ -> Left x
    )
{-# INLINE _Pending #-}

-- | @since 0.1
instance Display BatteryStatus where
  displayBuilder Charging = "Charging"
  displayBuilder Discharging = "Discharging"
  displayBuilder Full = "Full"
  displayBuilder Pending = "Pending"

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
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Percentage, b ~ Percentage) =>
  LabelOptic "percentage" k Battery Battery a b
  where
  labelOptic = lensVL $ \f (MkBattery _percentage _status) ->
    fmap (`MkBattery` _status) (f _percentage)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ BatteryStatus, b ~ BatteryStatus) =>
  LabelOptic "status" k Battery Battery a b
  where
  labelOptic = lensVL $ \f (MkBattery _percentage _status) ->
    fmap (MkBattery _percentage) (f _status)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Display Battery where
  displayBuilder bs =
    mconcat
      [ status,
        ": ",
        percentage
      ]
    where
      status = displayBuilder $ bs ^. #status
      percentage = displayBuilder $ bs ^. #percentage

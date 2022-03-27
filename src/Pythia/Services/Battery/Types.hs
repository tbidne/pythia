{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core types describing the battery.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.Types
  ( ChargeStatus (..),
    BatteryLevel,
    BatteryState (..),
  )
where

import Numeric.Data.Interval (LRInterval)
import Numeric.Data.Interval qualified as Interval
import Optics.TH qualified as OTH
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))

-- | Represents battery charging status.
--
-- @since 0.1.0.0
data ChargeStatus
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

OTH.makePrismLabels ''ChargeStatus

-- | Represents battery levels.
--
-- @since 0.1.0.0
type BatteryLevel = LRInterval 0 100 Int

-- | Full battery state, including level and status data.
--
-- @since 0.1.0.0
data BatteryState = MkBatteryState
  { -- | @since 0.1.0.0
    level :: BatteryLevel,
    -- | @since 0.1.0.0
    status :: ChargeStatus
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

OTH.makeFieldLabelsNoPrefix ''BatteryState

-- | @since 0.1.0.0
instance PrettyPrinter BatteryState where
  pretty bs = status <> ": " <> level <> "%"
    where
      status = pretty $ bs ^. #status
      level = show $ Interval.unLRInterval $ bs ^. #level

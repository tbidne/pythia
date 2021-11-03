-- | This module provides the core types describing the battery.
module System.Info.Power.Battery.Types
  ( ChargeStatus (..),
    BatteryLevel,
    BatteryState (..),
    BatteryProgram (..),
  )
where

import Data.Text (Text)
import Optics.Core (A_Lens, LabelOptic (..))
import Optics.Core qualified as O
import Smart.Data.Math.BoundedNat (BoundedNat)

-- | Represents battery charging status.
data ChargeStatus
  = Charging
  | Discharging
  | Full
  | Unknown Text
  deriving (Eq, Show)

-- | Represents battery levels
type BatteryLevel = BoundedNat 0 100

-- | Full battery state, including level and status data.
data BatteryState = MkBatteryState
  { -- | The level data.
    level :: BatteryLevel,
    -- | The status data.
    status :: ChargeStatus
  }
  deriving (Eq, Show)

instance
  LabelOptic
    "level"
    A_Lens
    BatteryState
    BatteryState
    BatteryLevel
    BatteryLevel
  where
  labelOptic = O.lens level (\state level' -> state {level = level'})

instance
  LabelOptic
    "status"
    A_Lens
    BatteryState
    BatteryState
    ChargeStatus
    ChargeStatus
  where
  labelOptic = O.lens status (\state status' -> state {status = status'})

-- | Determines how we should query the system for battery information.
-- The custom option assumes the same output format as UPower, i.e., the
-- output contains lines like:
--
-- @
-- percentage: 20%
-- state: <discharging|charging|fully-charged>
-- @
data BatteryProgram
  = UPower
  | Custom Text
  deriving (Eq, Show)

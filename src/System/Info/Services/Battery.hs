-- | This module exports battery related services.
--
-- @since 0.1.0.0
module System.Info.Services.Battery
  ( -- * Services

    -- ** Charge Status
    queryChargeStatus,
    BatteryChargeStatusApp (..),
    ChargeStatus (..),

    -- ** State
    queryBatteryState,
    BatteryStateApp (..),
    BatteryState (..),
    BatteryLevel,

    -- * Misc Types
    QueryResult,
    QueryError (..),
  )
where

import System.Info.Data (QueryError (..))
import System.Info.Services.Battery.ChargeStatus
  ( BatteryChargeStatusApp (..),
    ChargeStatus (..),
    queryChargeStatus,
  )
import System.Info.Services.Battery.State
  ( BatteryLevel,
    BatteryState (..),
    BatteryStateApp (..),
    queryBatteryState,
  )
import System.Info.ShellApp (QueryResult)

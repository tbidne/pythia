-- | This module exports battery related services.
--
-- @since 0.1.0.0
module Pythia.Services.Battery
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

import Pythia.Data (QueryError (..))
import Pythia.Services.Battery.ChargeStatus
  ( BatteryChargeStatusApp (..),
    ChargeStatus (..),
    queryChargeStatus,
  )
import Pythia.Services.Battery.State
  ( BatteryLevel,
    BatteryState (..),
    BatteryStateApp (..),
    queryBatteryState,
  )
import Pythia.ShellApp (QueryResult)

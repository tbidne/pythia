-- | This modules exports everything needed for retrieving battery
-- charge status.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.ChargeStatus
  ( -- * Query
    queryChargeStatus,

    -- * Types
    BatteryChargeStatusApp (..),
    ChargeStatus (..),
  )
where

import Pythia.Data (Command (..))
import Pythia.Prelude
import Pythia.Services.Battery.ChargeStatus.UPower qualified as UPower
import Pythia.Services.Battery.Types (ChargeStatus (..))
import Pythia.ShellApp (QueryResult, ShellApp (..))
import Pythia.ShellApp qualified as ShellApp

-- | Determines how we should query the system for charge status information.
-- The custom option assumes the same output format as UPower, i.e., the
-- output contains a line like:
--
-- @
-- state: \<discharging|charging|fully-charged\>
-- @
--
-- @since 0.1.0.0
data BatteryChargeStatusApp
  = -- | Uses the UPower utility.
    --
    -- @since 0.1.0.0
    ChargeStatusUPower
  | -- | Runs a custom script.
    --
    -- @since 0.1.0.0
    ChargeStatusCustom Text
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | This is the primary function that attempts to use the given
-- BatteryChargeStatusApp to retrieve battery information.
--
-- >>> queryChargeStatus UPower
-- Right Discharging
--
-- @since 0.1.0.0
queryChargeStatus :: BatteryChargeStatusApp -> IO (QueryResult ChargeStatus)
queryChargeStatus ChargeStatusUPower = ShellApp.runShellApp UPower.chargeStatusShellApp
queryChargeStatus (ChargeStatusCustom c) = ShellApp.runShellApp $ customShellApp c

-- Reuse UPower's parser
customShellApp :: Text -> ShellApp ChargeStatus
customShellApp cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    UPower.chargeStatusShellApp
